var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');
var assert = require('assert');

describe("Private Storgage Test", function () {
    this.timeout(10000);
    let args = null;
    let arg = null;
    let nodeId = null;
    let addr = null;
    let privFor1 = null;
    let privFor2 = null;
    let nodePubKey = null;

    before(function () {
        nodeId = fs.readFileSync('./cache/nodeId', 'utf8').replace('\n', '').replace(' ', '');
        arg = fs.readFileSync('./cache/arg', 'utf8').replace('\n', '').replace(' ', '');
        addr = fs.readFileSync('./cache/addr', 'utf8').replace('\n','').replace(' ','');
        nodePubKey = fs.readFileSync('./cache/nodePubKey', 'utf8').replace('\n', '').replace(' ', '');
        privFor1 = fs.readFileSync('./cache/privFor1', 'utf8').replace('\n', '').replace(' ', '');
        privFor2 = fs.readFileSync('./cache/privFor2', 'utf8').replace('\n', '').replace(' ', '');
        web3.setProvider(new web3.providers.HttpProvider('http://localhost:2200' + nodeId.toString()));
        web3.eth.defaultAccount = web3.eth.accounts[0];
        web3.personal.unlockAccount(web3.eth.accounts[0], "");
    });

    it('should set public storage contracts', function (done) {
        var account = web3.eth.accounts[0];
        var simplestorageContract = web3.eth.contract([{ "constant": false, "inputs": [{ "name": "x", "type": "uint256" }], "name": "set", "outputs": [], "payable": false, "type": "function" }, { "constant": true, "inputs": [], "name": "get", "outputs": [{ "name": "", "type": "uint256" }], "payable": false, "type": "function" }]);
        var txData = {
            from: account,
            gas: '500000',
            privateFrom: nodePubKey,
            privateFor: [privFor1, privFor2]
        };

        var awaitTx = function (contract, hash) {
            return function (err, res) {
                if (!res) {
                    setTimeout(function () {
                        web3.eth.getTransactionReceipt(hash, awaitTx(contract, hash))
                    }, 500);
                    return
                }
                const result = contract.get();
                console.log('Current value stored after set: ', JSON.parse(result))
                assert.equal(result, arg, "stored value not set as argument");
                done();
            }
        }

        console.log('Calling on contract with address: ', addr)
        var contract = simplestorageContract.at(addr)
        console.log('Current value stored before set: ', JSON.parse(contract.get()))
        const setPromise = new Promise(function (resolve, reject) {
            contract.set(arg, txData, function (err, hash) {
                resolve(hash);
            });
        });
        return setPromise
        .then(function(hash) {
            return new Promise(function (resolve, reject) {
                resolve(web3.eth.getTransactionReceipt(hash))
            });
        })
        .then(function (hash) {
            const setResult = JSON.parse(contract.get());
            console.log('Current value stored after set: ', JSON.parse(setResult));
            assert.equal(setResult, arg, "stored value not set as argument");
        }).catch(done);
    });
});