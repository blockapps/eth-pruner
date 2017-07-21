var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

var nodeId = process.argv[2]
var blocksFromLatest = process.argv[3]
if (nodeId === undefined || blocksFromLatest === undefined) {
  console.log('please run script with a nodeId and block.')
  console.log('ex: `node some-script.js <nodeId> <block>`')
  return;
}

web3.setProvider(new web3.providers.HttpProvider('http://localhost:850' + nodeId));
latestBlockNum = web3.eth.getBlock('latest').number
blockNum = latestBlockNum - blocksFromLatest
fs.writeFile('./test/selectedBlockNumber_'+nodeId, blockNum, function(err) {
  if(err) {
    return console.log(err);
  }
});
