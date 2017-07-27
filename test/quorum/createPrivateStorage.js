var fs = require('fs');
a = eth.accounts[0]
web3.eth.defaultAccount = a;

var simpleSource = 'contract simplestorage { uint public storedData; function simplestorage(uint initVal) { storedData = initVal; } function set(uint x) { storedData = x; } function get() constant returns (uint retVal) { return storedData; } }'
var simpleCompiled = web3.eth.compile.solidity(simpleSource);
var simpleRoot = Object.keys(simpleCompiled)[0];
var simpleContract = web3.eth.contract(simpleCompiled[simpleRoot].info.abiDefinition);
var simple1 = simpleContract.new(1, {from:web3.eth.accounts[0], data: simpleCompiled[simpleRoot].code, gas: 300000, privateFor: ["BULeR8JyUWhiuuCMU/HLA0Q5pzkYT+cHII3ZKBey3Bo="]}, function(e, contract) {
  if (e) {
    console.log("err creating contract", e);
  } else {
    if (!contract.address) {
      console.log("Contract 1 transaction send: TransactionHash: " + contract.transactionHash + " waiting to be mined...");
    } else {
      console.log("Contract 1 mined! Address: " + contract.address);
      console.log(contract);
      fs.writeFile('./contract1address', contract.address, function(err) {
        if(err) {
          return console.log(err);
        }
        console.log('Contract mined and wrote address: ' + contract.address);
        console.log('Current value stored: ', JSON.parse(contract.get()))
      });
    }
  }
});

var simple2 = simpleContract.new(2, {from:web3.eth.accounts[0], data: simpleCompiled[simpleRoot].code, gas: 300000, privateFor: ["oNspPPgszVUFw0qmGFfWwh1uxVUXgvBxleXORHj07g8="]}, function(e, contract) {
  if (e) {
    console.log("err creating contract", e);
  } else {
    if (!contract.address) {
      console.log("Contract 2 transaction send: TransactionHash: " + contract.transactionHash + " waiting to be mined...");
    } else {
      console.log("Contract 2 mined! Address: " + contract.address);
      console.log(contract);
      fs.writeFile('./contract2address', contract.address, function(err) {
        if(err) {
          return console.log(err);
        }
        console.log('Contract mined and wrote address: ' + contract.address);
        console.log('Current value stored: ', JSON.parse(contract.get()))
      });
    }
  }
});

var simple3 = simpleContract.new(3, {from:web3.eth.accounts[0], data: simpleCompiled[simpleRoot].code, gas: 300000, privateFor: ["1iTZde/ndBHvzhcl7V68x44Vx7pl8nwx9LqnM/AfJUg="]}, function(e, contract) {
  if (e) {
    console.log("err creating contract", e);
  } else {
    if (!contract.address) {
      console.log("Contract 3 transaction send: TransactionHash: " + contract.transactionHash + " waiting to be mined...");
    } else {
      console.log("Contract 3 mined! Address: " + contract.address);
      console.log(contract);
      fs.writeFile('./contract3address', contract.address, function(err) {
        if(err) {
          return console.log(err);
        }
        console.log('Contract mined and wrote address: ' + contract.address);
        console.log('Current value stored: ', JSON.parse(contract.get()))
      });
    }
  }
});
