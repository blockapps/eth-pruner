a = eth.accounts[0]
web3.eth.defaultAccount = a;

var simpleSource = 'contract simplestorage { uint public storedData; function simplestorage(uint initVal) { storedData = initVal; } function set(uint x) { storedData = x; } function get() constant returns (uint retVal) { return storedData; } }'
var simpleCompiled = web3.eth.compile.solidity(simpleSource);
var simpleRoot = Object.keys(simpleCompiled)[0];
var simpleContract = web3.eth.contract(simpleCompiled[simpleRoot].info.abiDefinition);
var simple1 = simpleContract.new(11, {from:web3.eth.accounts[0], data: simpleCompiled[simpleRoot].code, gas: 300000}, function(e, contract) {
  if (e) {
    console.log("err creating contract", e);
  } else {
    if (!contract.address) {
      console.log("Contract transaction 1 send: TransactionHash: " + contract.transactionHash + " waiting to be mined...");
    } else {
      console.log("Contract 1 mined! Address: " + contract.address);
      console.log(contract);
    }
  }
});

var simple2 = simpleContract.new(12, {from:web3.eth.accounts[0], data: simpleCompiled[simpleRoot].code, gas: 300000}, function(e, contract) {
  if (e) {
    console.log("err creating contract", e);
  } else {
    if (!contract.address) {
      console.log("Contract transaction 2 send: TransactionHash: " + contract.transactionHash + " waiting to be mined...");
    } else {
      console.log("Contract 2 mined! Address: " + contract.address);
      console.log(contract);
    }
  }
});

var simple3 = simpleContract.new(13, {from:web3.eth.accounts[0], data: simpleCompiled[simpleRoot].code, gas: 300000}, function(e, contract) {
  if (e) {
    console.log("err creating contract", e);
  } else {
    if (!contract.address) {
      console.log("Contract 3 transaction send: TransactionHash: " + contract.transactionHash + " waiting to be mined...");
    } else {
      console.log("Contract 3 mined! Address: " + contract.address);
      console.log(contract);
    }
  }
});
