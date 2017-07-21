var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

var nodeId = process.argv[2]
var addr = process.argv[3]
var arg = process.argv[4]
if (  nodeId === undefined
   || addr   === undefined
   || arg    === undefined) {
  console.log('please run script with a nodeId, address and arguments.')
  console.log('ex: `node some-script.js <nodeId> <addr> <arg>..`')
  return;
}

web3.setProvider(new web3.providers.HttpProvider('http://localhost:850' + nodeId));

var account = web3.eth.accounts[0];
var simplestorageContract = web3.eth.contract([{"constant":false,"inputs":[{"name":"x","type":"uint256"}],"name":"set","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"get","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"}]);
var txData = {
  from: account,
  gas: '500000'
};

var awaitTx = function(contract, hash) {
  return function(res) {
    if(!res) {
      setTimeout(function() {
        web3.eth.getTransactionReceipt(hashString , awaitTx(contract,hash))
      },2000);
      return
    }
    console.log('Current value stored after set: ', JSON.parse(contract.get()))
  }
}

var contract = simplestorageContract.at(addr)
console.log('Current value stored before set: ', JSON.parse(contract.get()))
contract.set(arg, txData, function(err, res) {
  setTimeout(function() {
    console.log('Current value stored after set: ', JSON.parse(contract.get()))
   }, 10000);
   console.log('waiting for tx to be mined')
});
