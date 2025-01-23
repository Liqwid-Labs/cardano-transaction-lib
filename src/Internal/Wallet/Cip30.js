/* global BROWSER_RUNTIME */

const stripSetTags = (hexEncodedCbor) => {
  // NOTE: The current version of CSL always encodes witness sets with the CBOR
  // set tag (258), which our version does not support. As both encodings are
  // supported until the next hard-fork, this only affects browser wallets.
  if (typeof BROWSER_RUNTIME !== 'undefined' && BROWSER_RUNTIME) {
    const cbor = require("cbor-web");
    const bdec = require("cbor-bigdecimal");
    bdec(cbor);
    const unwrapSetTag = (v) => v;
    const opts = {tags: { 258: unwrapSetTag }};

    const decoded = cbor.decodeFirstSync(Buffer.from(sample1, 'hex'), opts);
    const reencoded = cbor.encodeCanonical(decoded);
    return reencoded.toString('hex');

  } else {
    return hexEncodedCbor;
  }
};

exports._getNetworkId = conn => () => conn.getNetworkId();

exports._getUtxos = maybe => conn => () =>
  conn.getUtxos().then(res => (res === null ? maybe.nothing : maybe.just(res)));

exports._getCollateral = maybe => conn => () =>
  // yoroi will throw an error if no argument is provided
  // typhon will throw an error if the argument is not a string
  (conn.getCollateral
    ? conn.getCollateral("3000000")
    : conn.experimental.getCollateral("3000000")
  ).then(utxos =>
    utxos !== null && utxos.length ? maybe.just(utxos) : maybe.nothing
  );

exports._getBalance = conn => () => conn.getBalance();

exports._getAddresses = conn => () => conn.getUsedAddresses();

exports._getUnusedAddresses = conn => () => conn.getUnusedAddresses();

exports._getChangeAddress = conn => () => conn.getChangeAddress();

exports._getRewardAddresses = conn => () => conn.getRewardAddresses();

exports._signTx = txHex => conn => () =>
  conn.signTx(txHex, true).then(hexEncodedWitnessSet => {
    return stripSetTags(hexEncodedWitnessSet)
  }).catch(e => {
    throw JSON.stringify(e);
  });

exports._signData = address => payload => conn => () =>
  conn.signData(address, payload).catch(e => {
    throw JSON.stringify(e);
  });
