#!/bin/bash

genesisBlock="{
    \"config\": {
        \"chainId\": 15,
        \"homesteadBlock\": 0,
        \"eip155Block\": 0,
        \"eip158Block\": 0
    },
    \"difficulty\": \"20000\",
    \"gasLimit\": \"2100000\",
    \"alloc\": {
        \"7df9a875a174b3bc565e6424a0050ebc1b2d1d82\": { \"balance\": \"300000\" },
        \"f41c74c9ae680c1aa78f42e5647a62f353b7bdde\": { \"balance\": \"400000\" }
    }
}
"

bootSecretFile="boot-secret"
bootAddressFile="boot-address"

function createAccountOnNode {
    echo "creating account for node $1"
    echo "Unimplemented"
    exit
}

function initGethNodes {
    echo "using genesis file:" 
    echo "$1"
    echo "$1" > genesis.json

    # initialize 3 db paths
    for i in {0..2}
    do
        geth init --datadir "./datadirs/data_$i" genesis.json
        createAccountOnNode "$i"
    done
    rm genesis.json
}

function startBootNode {
    echo "starting bootnode. secret key in file: $1"
    echo "Unimplemented"
    exit
}

function startNodes {
    echo "starting nodes. Connecting to bootnode at address: $1"
    echo "Unimplemented"
    exit
}



# initialize 3 db paths
initGethNodes "$genesisBlock"

# start bootstrap node
startBootNode "$bootSecretFile"

# get enode key from bootstrap node
# start 3 nodes with enode pubkey
startNodes "$bootAddressFile"

#let all nodes mine and gain some ether
sleep 10

# run a contracts creation script on node 0
runScriptOnNode 0 "./test/js/createSimpleStorage.js"

# take node 0 down 
stopNode 0

# run a contracts creation script on node 1
runScriptOnNode 1 "./test/js/createSimpleStorage.js"

# run a method call script on node 2
runScriptOnNode 2 "./test/js/callSetOnNode1SimpleStorage.js"

# prune and backup Node data
pruneAndBackupNode 0

# start node A, now using pruned DB
startNode 0

# run a method call script on node A, interacting with 
runScriptOnNode 0 "./test/js/callGetOnNode1SimpleStorage.js"

