var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

var nodeId = process.argv[2]
var addr = process.argv[3]
var blockNumber = process.argv[4]
if ( nodeId      === undefined
  || addr        === undefined
  || blockNumber === undefined) {
  console.log('please run script with a nodeId, contract address, and block number.')
  console.log('ex: `node some-script.js <nodeId> <addr> <block-number> ..`')
  return;
}

web3.setProvider(new web3.providers.HttpProvider('http://localhost:850' + nodeId));

var account = web3.eth.accounts[0];
var simplestorageContract = web3.eth.contract([{"constant":false,"inputs":[{"name":"x","type":"uint256"}],"name":"set","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"get","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"}]);

console.log('  Calling on contract with address: ',addr)
var contract = simplestorageContract.at(addr)
console.log('  Current value on contract: ', JSON.parse(contract.get()))
console.log('  Value on contract at block ' + blockNumber + ' is: ',
            JSON.parse(contract.get.call(blockNumber)))
