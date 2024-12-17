/* global BROWSER_RUNTIME */
const cbor2 = require('cbor2');
const cbor2Encoder = require('cbor2/encoder');
const cbor2Utils = require('cbor2/utils');

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
    // Decode the witness set and re-encode it stripping out the set tag (258).
    const decoded = cbor2.decode(cbor2Utils.hexToU8(hexEncodedWitnessSet));
    const previousSetEncoder = cbor2Encoder.registerEncoder(Set, (obj) => [undefined, [...obj]]);
    const encodedWithoutSetTag = cbor2.encode(decoded);
    // Restore the previous encoder for Sets.
    cbor2Encoder.registerEncoder(Set, previousSetEncoder);
    return cbor2Utils.u8toHex(encodedWithoutSetTag);
  }).catch(e => {
    throw JSON.stringify(e);
  });

exports._signData = address => payload => conn => () =>
  conn.signData(address, payload).catch(e => {
    throw JSON.stringify(e);
  });
