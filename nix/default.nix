{ pkgs }:
{ src
  # The name of the project, used to generate derivation names
, projectName
  # The `package.json` for the project. It is *highly* recommended to pass this
  # in explicitly, even if it can be derived from the `src` argument. By doing
  # so, you will prevent frequent rebuilds of your generated `node_modules`
, packageJson ? "${src}/package.json"
  # The `package-lock.json` for the project. It is *highly* recommended to pass
  # this in explicitly, even if it can be derived from the `src` argument. By
  # doing so, you will prevent frequent rebuilds of your generated `node_modules`
, packageLock ? "${src}/package-lock.json"
  # If warnings generated from project source files will trigger a build error
, strictComp ? true
  # Warnings from `purs` to silence during compilation, independent of `strictComp`
, censorCodes ? [ "UserDefinedWarning" ]
  # The version of node to use across all project components
, nodejs ? pkgs.nodejs-18_x
  # Autogenerated Nix from `spago2nix generate`
, spagoPackages ? "${src}/spago-packages.nix"
  # Extra Purescript sources to build and provide in the `devShell` as `extraSourcesDir`
, extraSources ? [ ]
, extraSourcesDir ? ".extras"
  # Data directory to add to the build and provide in the `devShell` as `dataDir`
, data ? [ ]
, dataDir ? "data"
  # Configuration that will be used to generate a `devShell` for the project
, shell ? { }
, ...
}:
let
  inherit (pkgs) system;

  purs = pkgs.easy-ps.purs-0_14_9;

  spagoPkgs = import spagoPackages { inherit pkgs; };

  mkNodeEnv = { withDevDeps ? true }: import
    (pkgs.runCommand "node-packages-${projectName}"
      {
        buildInputs = [ pkgs.nodePackages.node2nix ];
      } ''
      mkdir $out
      cd $out
      cp ${packageLock} ./package-lock.json
      cp ${packageJson} ./package.json
      node2nix ${pkgs.lib.optionalString withDevDeps "--development" } \
        --lock ./package-lock.json -i ./package.json
    '')
    { inherit pkgs nodejs system; };

  mkNodeModules = { withDevDeps ? true }:
    let
      nodeEnv = mkNodeEnv { inherit withDevDeps; };
      modules = pkgs.callPackage
        (_:
          nodeEnv // {
            shell = nodeEnv.shell.override {
              # see https://github.com/svanderburg/node2nix/issues/198
              buildInputs = [ pkgs.nodePackages.node-gyp-build ];
            };
          });
    in
    (modules { }).shell.nodeDependencies;

  projectNodeModules = mkNodeModules { };

  # Constructs a development environment containing various tools to work on
  # Purescript projects. The resulting derivation can be used as a `devShell` in
  # your flake outputs
  #
  # All arguments are optional
  shellFor =
    {
      # Extra packages to include in the shell environment
      packages ? [ ]
      # Passed through to `pkgs.mkShell.inputsFrom`
    , inputsFrom ? [ ]
      # Passed through to `pkgs.mkShell.shellHook`
    , shellHook ? ""
      # One of `purs-tidy` or `purty` to format Purescript sources
    , formatter ? "purs-tidy"
      # Whether or not to include `purescript-language-server`
    , pursls ? true
      # Generated `node_modules` in the Nix store. Can be passed to have better
      # control over individual project components
    , nodeModules ? projectNodeModules
      # If `true`, `npm i` will only write to your `package-lock.json` instead
      # of installing to a local `node_modules`
    , packageLockOnly ? false
      # If `true`, all of CTL's runtime dependencies will be added to the
      # shell's `packages`. These packages are *required* if you plan on running
      # Plutip tests in your local shell environment (that is, not using Nix
      # directly as with `runPlutipTest`). Make sure you have applied
      # `overlays.runtime` or otherwise added the runtime packages to your
      # package set if you select this option!
    , withRuntime ? true
      # If `true`, the `chromium` package from your package set will be made
      # available in the shell environment. This can help with ensuring that
      # any e2e tests that you write and run with `Contract.Test.E2E` are
      # reproducible
    , withChromium ? pkgs.stdenv.isLinux
    }:
      assert pkgs.lib.assertOneOf "formatter" formatter [ "purs-tidy" "purty" ];
      with pkgs.lib;
      pkgs.mkShell {
        inherit packages inputsFrom;
        buildInputs = builtins.concatLists
          [
            [
              nodeModules
              purs
              nodejs
              pkgs.easy-ps.spago
              pkgs.easy-ps.${formatter}
              pkgs.easy-ps.pscid
              pkgs.easy-ps.psa
              pkgs.easy-ps.spago2nix
              pkgs.nodePackages.node2nix
              pkgs.unzip
              # Required to fix initdb locale issue in shell
              # https://github.com/Plutonomicon/cardano-transaction-lib/issues/828
              # Well, not really, as we set initdb locale to C for all cases now
              # Anyway, seems like it's good to have whole set of locales in the shell
              pkgs.glibcLocales
            ]

            (lists.optional pursls pkgs.easy-ps.purescript-language-server)

            (lists.optional withChromium pkgs.chromium)

            (
              lists.optional withRuntime (
                [
                  pkgs.ogmios
                  pkgs.plutip-server
                  pkgs.kupo
                ]
              )
            )
          ];
        shellHook = ''
          export NODE_PATH="${nodeModules}/lib/node_modules"
          export PATH="${nodeModules}/bin:$PATH"
          ${pkgs.lib.optionalString packageLockOnly "export NPM_CONFIG_PACKAGE_LOCK_ONLY=true"}
          ${linkExtraSources}
          ${linkData}
        ''
        + shellHook;
      };

  # Extra sources
  extra-sources = pkgs.linkFarm "extra-sources" (builtins.map (drv: { name = drv.name; path = "${drv}/src"; }) extraSources);
  hasExtraSources = builtins.length extraSources > 0;
  linkExtraSources = pkgs.lib.optionalString hasExtraSources ''
    if [ -e ./${extraSourcesDir} ]; then rm ./${extraSourcesDir}; fi
    ln -s ${extra-sources} ./${extraSourcesDir}
  '';

  # Data
  data-drv = pkgs.linkFarm "data" data;
  hasData = builtins.length data > 0;
  linkData = pkgs.lib.optionalString hasData ''
    if [ -e ./${dataDir} ]; then rm ./${dataDir}; fi
    ln -s ${data-drv} ./${dataDir}
  '';

  # Compiles your Purescript project and copies the `output` directory into the
  # Nix store. Also copies the local sources to be made available later as `purs`
  # does not include any external files to its `output` (if we attempted to refer
  # to absolute paths from the project-wide `src` argument, they would be wrong)
  buildPursProject =
    {
      # Can be used to override the name given to the resulting derivation
      name ? projectName
      # Generated `node_modules` in the Nix store. Can be passed to have better
      # control over individual project components
    , nodeModules ? projectNodeModules
    , ...
    }:
    pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = [
        nodeModules
        spagoPkgs.installSpagoStyle
        pkgs.easy-ps.psa
      ];
      nativeBuildInputs = [
        purs
        pkgs.easy-ps.spago
      ];
      unpackPhase = ''
        export HOME="$TMP"
        export NODE_PATH="${nodeModules}/lib/node_modules"
        export PATH="${nodeModules}/bin:$PATH"
        cp -r $src .
        ${linkExtraSources}
        ${linkData}
        install-spago-style
      '';
      buildPhase = ''
        set -vox
        psa ${pkgs.lib.optionalString strictComp "--strict" } \
          --censor-lib \
          --is-lib=.spago ".spago/*/*/src/**/*.purs" ${pkgs.lib.optionalString hasExtraSources ''--is-lib=./${extraSourcesDir} "${extraSourcesDir}/*/**/*.purs"''} \
          --censor-codes=${builtins.concatStringsSep "," censorCodes} "./**/*.purs" \
          -gsourcemaps,js
      '';
      # We also need to copy all of `src` here, since compiled modules in `output`
      # might refer to paths that will point to nothing if we use `src` directly
      # in other derivations (e.g. when using `fs.readFileSync` inside an FFI
      # module)
      installPhase = ''
        mkdir $out
        mv output $out/
        cp -r $src/* $out/
        ${pkgs.lib.optionalString hasExtraSources ''cp -r ./${extraSourcesDir} $out/''}
        ${pkgs.lib.optionalString hasData ''cp -r ./${dataDir} $out/''}
      '';
    };

  project = buildPursProject { };

  # Runs a test written in Purescript using NodeJS.
  runPursTest =
    {
      # The name of the main Purescript module
      testMain ? "Test.Main"
      # Can be used to override the name of the resulting derivation
    , name ? "${projectName}-check"
      # Generated `node_modules` in the Nix store. Can be passed to have better
      # control over individual project components
    , nodeModules ? projectNodeModules
      # Additional variables to pass to the test environment
    , env ? { }
      # Passed through to the `buildInputs` of the derivation. Use this to add
      # additional packages to the test environment
    , buildInputs ? [ ]
    , ...
    }: pkgs.runCommand "${name}"
      (
        {
          buildInputs = [ project nodeModules ] ++ buildInputs;
          NODE_PATH = "${nodeModules}/lib/node_modules";
        } // env
      )
      # spago will attempt to download things, which will fail in the
      # sandbox, so we can just use node instead
      # (idea taken from `plutus-playground-client`)
      ''
        cd ${project}
        ${nodejs}/bin/node --enable-source-maps -e 'require("./output/${testMain}").main()'
        touch $out
      '';

  # Runs a test using Plutip. Takes the same arguments as `runPursTest`
  #
  # NOTE: You *must* either use CTL's `overlays.runtime` or otherwise make the
  # the following required `buildInputs` available in your own package set:
  #
  #  - `ogmios`
  #  - `kupo`
  #  - `plutip-server`
  #
  runPlutipTest =
    args:
    runPursTest (
      args // {
        buildInputs = with pkgs; [
          ogmios
          plutip-server
          kupo
        ]
        ++ (args.buildInputs or [ ]);
      }
    );

  runE2ETest =
    {
      # The name of the main Purescript module
      testMain ? "Test.Ctl.E2E"
      # Can be used to override the name of the resulting derivation
    , name ? "${projectName}-e2e"
      # Generated `node_modules` in the Nix store. Can be passed to have better
      # control over individual project components
    , nodeModules ? projectNodeModules
      # Additional variables to pass to the test environment
    , env ? { }
      # Passed through to the `buildInputs` of the derivation. Use this to add
      # additional packages to the test environment
    , buildInputs ? [ ]
    , ...
    }@args:
    let
      bundledPursProject = bundlePursProject {
        main = "Ctl.Examples.ByUrl";
        entrypoint = "examples/index.js";
      };
      # We need fonts if we are going to use chromium
      etc_fonts =
        let
          fonts = with pkgs; [
            dejavu_fonts
            freefont_ttf
            liberation_ttf
          ];
          cache = pkgs.makeFontsCache { inherit (pkgs) fontconfig; fontDirectories = fonts; };
          config = pkgs.writeTextDir "conf.d/00-nixos-cache.conf" ''<?xml version='1.0'?>
            <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
            <fontconfig>
              ${builtins.concatStringsSep "\n" (map (font: "<dir>${font}</dir>") fonts)}
              <cachedir>${cache}</cachedir>
            </fontconfig>
          '';
        in
        pkgs.buildEnv { name = "etc-fonts"; paths = [ "${pkgs.fontconfig.out}/etc/fonts" config ]; };
      # We use bubblewrap to populate /etc/fonts.
      # We use ungoogled-chromium because chromium some times times out on Hydra.
      #
      # Chromium wrapper code was provided to us by Las Safin (thanks)
      chromium = pkgs.writeShellScriptBin "chromium" ''
        env - ${pkgs.bubblewrap}/bin/bwrap \
          --unshare-all \
          --share-net \
          --ro-bind /nix/store /nix/store \
          --bind /build /build \
          --uid 1000 \
          --gid 1000 \
          --proc /proc \
          --dir /tmp \
          --dev /dev \
          --setenv TMPDIR /tmp \
          --setenv XDG_RUNTIME_DIR /tmp \
          --bind . /data \
          --chdir /data  \
          --ro-bind ${etc_fonts} /etc/fonts \
          -- ${pkgs.ungoogled-chromium}/bin/chromium \
            --no-sandbox \
            --disable-setuid-sandbox \
            --disable-gpu \
            "$@"
      '';
    in
    pkgs.runCommand "${name}"
      ({
        buildInputs = with pkgs; [
          project
          nodeModules
          ogmios
          kupo
          plutip-server
          chromium
          python38 # To serve bundled CTL
          # Utils needed by E2E test code
          which # used to check for browser availability
          gnutar # used unpack settings archive within E2E test code
          curl # used to query for the web server to start (see below)
        ] ++ (args.buildInputs or [ ]);
        NODE_PATH = "${nodeModules}/lib/node_modules";
      } // env)
      ''
        chmod -R +rw .

        source ${project}/test/e2e-ci.env

        export E2E_SETTINGS_ARCHIVE="${project}/test-data/empty-settings.tar.gz"
        export E2E_CHROME_USER_DATA="./test-data/chrome-user-data"
        export E2E_TEST_TIMEOUT=200
        export E2E_BROWSER=${chromium}/bin/chromium # use custom bwrap-ed chromium
        export E2E_NO_HEADLESS=false
        export PLUTIP_PORT=8087
        export OGMIOS_PORT=1345
        export E2E_EXTRA_BROWSER_ARGS="--disable-web-security"

        python -m http.server 4008 --directory ${bundledPursProject}/dist &
        until curl -S http://127.0.0.1:4008/index.html &>/dev/null; do
          echo "Trying to connect to webserver...";
          sleep 0.1;
        done;


        ${nodejs}/bin/node --enable-source-maps -e 'require("${project}/output/${testMain}").main()' e2e-test run
        mkdir $out
      ''
  ;

  # Bundles a Purescript project using Webpack, typically for the browser
  bundlePursProject =
    {
      # Can be used to override the name given to the resulting derivation
      name ? "${projectName}-bundle-" +
        (if browserRuntime then "web" else "nodejs")
      # The Webpack `entrypoint`
    , entrypoint ? "index.js"
      # The main Purescript module
    , main ? "Main"
      # If this bundle is being produced for a browser environment or not
    , browserRuntime ? true
      # Path to the Webpack config to use
    , webpackConfig ? "webpack.config.js"
      # The name of the bundled JS module that `spago bundle-module` will produce
    , bundledModuleName ? "output.js"
      # Generated `node_modules` in the Nix store. Can be passed to have better
      # control over individual project components
    , nodeModules ? projectNodeModules
      # If the spago bundle-module output should be included in the derivation
    , includeBundledModule ? false
    , ...
    }: pkgs.runCommand "${name}"
      {
        buildInputs = [
          nodejs
          nodeModules
          project
        ];
        nativeBuildInputs = [
          purs
          pkgs.easy-ps.spago
        ];
      }
      ''
        export HOME="$TMP"
        export NODE_PATH="${nodeModules}/lib/node_modules"
        export PATH="${nodeModules}/bin:$PATH"
        ${pkgs.lib.optionalString browserRuntime "export BROWSER_RUNTIME=1"}
        cp -r ${project}/* .
        chmod -R +rwx .
        spago bundle-module --no-install --no-build -m "${main}" \
          --to ${bundledModuleName}
        mkdir -p ./dist
        ${pkgs.lib.optionalString includeBundledModule "cp ${bundledModuleName} ./dist"}
        webpack --mode=production -c ${webpackConfig} -o ./dist \
          --entry ./${entrypoint}
        mkdir $out
        mv dist $out
      '';

  pursDocsSearchNpm =
    let
      fakePackage = builtins.toJSON {
        name = "pursDocsSearch";
        version = "0.0.0";
        dependencies = { "purescript-docs-search" = "0.0.12"; };
      };
      fakePackageLock = builtins.toJSON {
        requires = true;
        lockfileVersion = 1;
        dependencies = {
          punycode = {
            version = "2.1.1";
            resolved = "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz";
            integrity = "sha512-XRsRjdf+j5ml+y/6GKHPZbrF/8p2Yga0JPtdqTIY2Xe5ohJPD9saDJJLPvp9+NSBprVvevdXZybnj2cv8OEd0A==";
          };
          purescript-docs-search = {
            version = "0.0.12";
            resolved = "https://registry.npmjs.org/purescript-docs-search/-/purescript-docs-search-0.0.12.tgz";
            integrity = "sha512-NdhQ3AxbKR2wO+WT2fGa8Rw26JydL6Bgnf73WOazmlfHt4uszblYqiWfaZygyUMOQFnXtpqz5TQj6DW6nk4nEg==";
          };
        };
      };
    in
    import
      (pkgs.runCommand "purescript-docs-search-npm"
        {
          buildInputs = [ pkgs.nodePackages.node2nix ];
        }
        ''
          mkdir $out
          cd $out
          cat > package.json <<EOF
            ${fakePackage}
          EOF
          cat > package-lock.json <<EOF
            ${fakePackageLock}
          EOF
          node2nix --lock ./package-lock.json -i ./package.json
        '')
      { inherit pkgs nodejs system; };

  buildPursDocs =
    { name ? "${projectName}-docs"
    , format ? "html"
    , ...
    }@args:
    (buildPursProject args).overrideAttrs
      (oas: {
        inherit name;
        buildPhase = ''
          purs docs --format ${format} "./**/*.purs" ".spago/*/*/src/**/*.purs"
        '';
        installPhase = ''
          mkdir $out
          cp -r generated-docs $out
          cp -r output $out
          cp -r $src/src $out
        '';
      });

  # Builds all of the documentation for your Purescript project (including deps)
  # and creates a searchable index for them
  buildSearchablePursDocs =
    {
      # Passed to the `--package-name` argument of `purescript-docs-search`
      packageName ? projectName
    , ...
    }:
    pkgs.runCommand "${projectName}-searchable-docs"
      {
        buildInputs = [ spagoPkgs.installSpagoStyle ];
      }
      ''
        export NODE_PATH="${pursDocsSearchNpm.nodeDependencies}/lib/node_modules"
        export PATH="${pursDocsSearchNpm.nodeDependencies}/bin:$PATH"
        cp -r ${buildPursDocs { }}/{generated-docs,output,src} .
        install-spago-style
        chmod -R +rwx .
        purescript-docs-search build-index --package-name ${packageName} --source-files 'src/**/*.purs'
        mkdir $out
        cp -r generated-docs $out
      '';

  # Creates a flakes-compatible `apps` output to serve a searchable index of all
  # project docs (including dependencies) locally. For example
  #
  # ```
  #   apps = perSystem (system: {
  #     docs = (psProjectFor system).launchSearchablePursDocs { port = 9090; };
  #   });
  # ```
  #
  # You can then invoke `nix run .#docs` to serve the documentation index locally
  # and visit `localhost:9090` to browse them
  launchSearchablePursDocs =
    {
      # If you are already building your docs (e.g. as part of your flake
      # `packages`), you can pass them here. Otherwise, `buildSearchablePursDocs`
      # will be invoked
      builtDocs ? null
      # The port to run the local HTTP server on
    , port ? 8080
    , ...
    }:
    let
      binPath = "docs-server";
      docs =
        if builtDocs == null
        then buildSearchablePursDocs { }
        else builtDocs;
      script = pkgs.writeShellApplication {
        name = binPath;
        runtimeInputs = [
          pkgs.nodejs-18_x
          pkgs.nodePackages.http-server
        ];
        text =
          ''
            ${pkgs.nodePackages.http-server}/bin/http-server \
              --port ${builtins.toString port} ${docs}/generated-docs/html
          '';
      };
    in
    {
      type = "app";
      program = "${script}/bin/${binPath}";
    };

in
{
  inherit
    buildPursProject runPursTest runPlutipTest runE2ETest bundlePursProject
    buildPursDocs buildSearchablePursDocs launchSearchablePursDocs
    purs nodejs mkNodeModules;
  devShell = shellFor shell;
  compiled = project;
  nodeModules = projectNodeModules;
}
