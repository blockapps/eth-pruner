var account = web3.eth.accounts[0];
var simplestorageContract = web3.eth.contract([{"constant":false,"inputs":[{"name":"x","type":"uint256"}],"name":"set","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"get","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"}]);
var simplestorage = simplestorageContract.new(
     {
            from: account,
            data: '0x6060604052341561000f57600080fd5b5b60ce8061001e6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806360fe47b11460475780636d4ce63c146067575b600080fd5b3415605157600080fd5b60656004808035906020019091905050608d565b005b3415607157600080fd5b60776098565b6040518082815260200191505060405180910390f35b806000819055505b50565b6000805490505b905600a165627a7a723058202b52e47667add987779d71e6d3fe5e240d788b60134481246340b9fd1675ce1d0029',
            gas: '500000'
          }, function (e, contract){
                console.log(e, JSON.stringify(contract,null,2));
                if (typeof contract.address !== 'undefined') {
                           console.log('Contract mined! address: ' + contract.address + ' transactionHash: ' + contract.transactionHash);
                      }
             })

