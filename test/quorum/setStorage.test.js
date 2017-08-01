var Web3 = require('web3');
var web3 = new Web3();
var fs = require('fs');

describe("Public Storgage Test", function () {
    this.timeout(10000);
    let args = null;
    let arg = null;
    let nodeId = null;
    let addr = null;
    
    before(function() {
        nodeId = fs.readFileSync('./cache/nodeId', "utf8");
        arg = fs.readFileSync('./cache/arg', "utf8");
        addr = fs.readFileSync('./cache/addr', 'utf8').replace('\n', '').replace(' ', '');
        web3.setProvider(new web3.providers.HttpProvider('http://localhost:2200' + nodeId));
        web3.eth.defaultAccount = web3.eth.accounts[0];
        web3.personal.unlockAccount(web3.eth.accounts[0], "");
    });

    it('should set public storage contracts', function (done) {
        var account = web3.eth.accounts[0];
        var simplestorageContract = web3.eth.contract([{ "constant": false, "inputs": [{ "name": "x", "type": "uint256" }], "name": "set", "outputs": [], "payable": false, "type": "function" }, { "constant": true, "inputs": [], "name": "get", "outputs": [{ "name": "", "type": "uint256" }], "payable": false, "type": "function" }]);
        var txData = {
            from: account,
            gas: '500000'
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
        const result = contract.set(arg, txData, function (err, res) {
            awaitTx(contract, res)()
            console.log('waiting for tx to be mined')
        });
    });
});