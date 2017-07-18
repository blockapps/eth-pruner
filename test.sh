#!/bin/bash

bootSecretFile="./test/bootnode/boot-secret"
bootAddress=$(<"./test/bootnode/boot-address")
nodeCoinbases=("5414f0e462f6013998550a728371d67eeed0bb6d" "a82fd9c16f782676e46f3c15d16b63deae5e5afd" "06037e1daa9f4c356b84b87dbcbbc23a562f978e")
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
        \"${nodeCoinbases[0]}\": { \"balance\": \"300000000000000000000000\" },
        \"${nodeCoinbases[1]}\": { \"balance\": \"300000000000000000000000\" },
        \"${nodeCoinbases[2]}\": { \"balance\": \"300000000000000000000000\" }
    }
}
"
gethPIDs=()


function initGethNodes {
    echo "Using genesis file:" 
    echo "$1"
    echo "$1" > genesis.json

    # initialize 3 db paths
    for i in {0..2}
    do
        mkdir -p "./datadirs/data_$i/logs"
        geth init --datadir "./datadirs/data_$i" genesis.json > "./datadirs/data_$i/logs/init.log"
        cp -r "./test/keystore" "./datadirs/data_$i"
    done
    rm genesis.json
}

function startBootNode {
    echo "Starting bootnode with secret key file: $1"
    bootSecretKey=$(<"$1" )
    bootnode -nodekey "./test/bootnode/boot-secret" -addr ":30299"> "datadirs/bootnode.log" 2>&1 &
}

function startNode {
    echo "Starting node $1. Connecting to bootnode at address: $2"
    geth --datadir "./datadirs/data_$i" --networkid 15 --port "3030$1" --mine --minerthreads=1 --etherbase="${nodeCoinbases[$1]}" --verbosity 11 --bootnodes "enode://$2@127.0.0.1:30299" >> "./datadirs/data_$i/logs/geth.log" 2>&1 &
    gethPIDs[$1]="$!"
}
function startNodes {
    for i in {0..2}
    do
        startNode $i "$bootAddress"
    done
}

function runScriptOnNode {
    geth attach "./datadirs/data_$1/geth.ipc" --exec "$2"
}

function stopNode {
  echo "Stopping node: $1"
  kill -9 ${gethPIDS[$1]}
}

function pruneAndBackupNode {
  echo "pruneAndBackupNode: Not Implemented"
}

# initialize 3 db paths
initGethNodes "$genesisBlock"

# start bootstrap node
startBootNode "$bootSecretFile"

# get enode key from bootstrap node
# start 3 nodes with enode pubkey
startNodes "$bootAddressFile"

# sleep to let nodes start
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
startNode 0 $bootAddress

# run a method call script on node A, interacting with 
runScriptOnNode 0 "./test/js/callGetOnNode1SimpleStorage.js"

