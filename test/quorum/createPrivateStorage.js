var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

var nodeId = process.argv[2]
var nodePubKey = process.argv[3];
var privFor1 = process.argv[4];
var privFor2 = process.argv[5];
if (nodeId === undefined || nodePubKey === undefined|| privFor1 === undefined
|| privFor2 === undefined) {
  console.log('please run script with a nodeId and public key. (ex: `node <script> 0 <nodePubKey> <nodePrivFor1> <nodePrivFor2>`')
  return;
}

console.log("PRIVATE FROM", nodePubKey);
web3.setProvider(new web3.providers.HttpProvider('http://localhost:2200' + nodeId));

var account = web3.eth.accounts[0];
var simplestorageContract = web3.eth.contract([{"constant":false,"inputs":[{"name":"x","type":"uint256"}],"name":"set","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"get","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"}]);
var txData = {
  from: account,
  data: '0x60606040525b5b5b60ce806100156000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806360fe47b11460475780636d4ce63c146067575b600080fd5b3415605157600080fd5b60656004808035906020019091905050608d565b005b3415607157600080fd5b60776098565b6040518082815260200191505060405180910390f35b806000819055505b50565b6000805490505b905600a165627a7a723058204e883af6ea22f200009c39ab51f76dd0b1fcb94593b2c07ecc517c395a52e20f0029',
  gas: '500000',
  privateFrom: nodePubKey,
  privateFor: [privFor1, privFor2]
};
var contractCallback = function (e, contract) {
  if (typeof contract.address !== 'undefined') {
    fs.writeFile('./contracts/privateAddress' + nodeId, contract.address, function(err) {
      if(err) {
        return console.log(err);
      }
      console.log('Contract mined and wrote address: ' + contract.address);
      console.log('Current value stored: ', JSON.parse(contract.get()))
    });
  }
}
simplestorageContract.new(txData, contractCallback)
