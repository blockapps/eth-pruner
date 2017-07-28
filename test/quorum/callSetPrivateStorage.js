var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

var nodeId = process.argv[2]
var nodePubKey = process.argv[3];
var addr = process.argv[4];
var arg = process.argv[5];
var privFor1 = process.argv[6];
var privFor2 = process.argv[7];
if (  nodeId === undefined
   || addr   === undefined
   || arg    === undefined
   || nodePubKey === undefined
   || privFor1 === undefined
   || privFor2 === undefined) {
  console.log('please run script with a nodeId, address and arguments.')
  console.log('ex: `node some-script.js <nodeId> <nodePubKey> <addr> <arg> <privKeyFor1> <privKeyFor2>..`')
  return;
}

console.log("PRIVATE FROM", nodePubKey);

web3.setProvider(new web3.providers.HttpProvider('http://localhost:2200' + nodeId));

var account = web3.eth.accounts[0];
var simplestorageContract = web3.eth.contract([{"constant":false,"inputs":[{"name":"x","type":"uint256"}],"name":"set","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"get","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"}]);
var txData = {
  from: account,
  gas: '500000',
  privateFrom: nodePubKey,
  privateFor: [privFor1, privFor2]
};

var awaitTx = function(contract, hash) {
  return function(err,res) {
    if(!res) {
      setTimeout(function() {
        web3.eth.getTransactionReceipt(hash, awaitTx(contract,hash))
      },500);
      return
    }
    console.log('Current value stored after set: ', JSON.parse(contract.get()))
  }
}

console.log('Calling on contract with address: ',addr)
var contract = simplestorageContract.at(addr)
console.log('Current value stored before set: ', JSON.parse(contract.get()))
contract.set(arg, txData, function(err, res) {
  awaitTx(contract,res)()
  console.log('waiting for tx to be mined')
});
