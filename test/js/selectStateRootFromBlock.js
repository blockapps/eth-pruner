var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

var nodeId = process.argv[2]
var block = process.argv[3]
if (nodeId === undefined || block === undefined) {
  console.log('please run script with a nodeId and block.')
  console.log('ex: `node some-script.js <nodeId> <block>`')
  return;
}

web3.setProvider(new web3.providers.HttpProvider('http://localhost:850' + nodeId));
blockDetails = web3.eth.getBlock(block)
if (blockDetails) {
  fs.writeFile('./test/stateroot1', blockDetails.stateRoot, function(err) {
    if(err) {
      return console.log(err);
    }
  });
}
