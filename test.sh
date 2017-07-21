#!/bin/bash

bootSecretFile="./test/bootnode/boot-secret"
bootAddress=$(<"./test/bootnode/boot-address")
coinbase="005519a3f7c402ddc3035cd6e34f22b80a7f01ac" 
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
        \"$coinbase\": { \"balance\": \"900000000000000000000000\" }
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
        geth init --datadir "./datadirs/data_$i" genesis.json > "./datadirs/data_$i/logs/init.log" 2>&1 
        cp -r "./test/keystore" "./datadirs/data_$i"
    done
    rm genesis.json
}

function startBootNode {
    echo
    echo "Starting bootnode with secret key file: $1"
    bootSecretKey=$(<"$1" )
    bootnode -nodekey "./test/bootnode/boot-secret" -addr ":30299"> "datadirs/bootnode.log" 2>&1 &
}

function startNode {
    echo "Starting node $1. Connecting to bootnode at address: $2"
    port="3030$1"
    rpcPort="850$1"
    datadir="./datadirs/data_$1"
    bootnodeAddr="enode://$2@127.0.0.1:30299"
    geth --datadir "$datadir" --unlock "$coinbase" --password "./test/password" --rpc --rpcport "$rpcPort" --networkid 15 --port "$port" --mine --minerthreads=1 --etherbase="$coinbase" --verbosity 11 --bootnodes "$bootnodeAddr" >> "$datadir/logs/geth.log" 2>&1 &
    gethPIDs[$1]="$!"
}
function startNodes {
    for i in {0..2}
    do
        startNode $i "$bootAddress"
    done
}

function runScriptOnNode {
  echo
  echo "running script on node $2"
  scriptPath=$1
  shift
  node "$scriptPath" "$@"
}

function stopNode {
  echo
  echo "Stopping node: $1"
  kill -HUP ${gethPIDs[$1]}
}

function pruneAndBackupNode {
  echo
  echo "Pruning and Backing Up node $1"

  dir="./datadirs/data_$1/geth"
  blockNum=$(<"./test/selectedBlockNumber_$1")
  cd $dir
  prune "$blockNum"
  mv chaindata chaindata_old
  mv chaindata_new chaindata
  cd ../../..
}

function cleanUp {
  # rm -rf ./datadirs
  rm -rf ./test/contracts
  rm ./test/selectedBlockNumber_*
  killall geth bootnode
}

# initialize 3 db paths
initGethNodes "$genesisBlock"

# start bootstrap node
startBootNode "$bootSecretFile"
sleep 2

# get enode key from bootstrap node
# start 3 nodes with enode pubkey
startNodes "$bootAddressFile"

# sleep to let nodes start
echo
echo "Waiting 20s for nodes to start and begin mining blocks"
sleep 20

mkdir ./test/contracts

# run a contracts creation script on node 0
runScriptOnNode "./test/js/createSimpleStorage.js" 0

# run a method call script on node 2
addr=$(<"./test/contracts/address0")
runScriptOnNode "./test/js/callSetSimpleStorage.js" 2 $addr 10

echo
echo "Let some blocks build before stopping node 0"
sleep 20

# save the stateroot of the latest block to a file
runScriptOnNode "./test/js/selectBlock.js" 0 6

# take node 0 down 
stopNode 0

# run a contracts creation script on node 1
runScriptOnNode "./test/js/createSimpleStorage.js" 1

# run a method call script on node 2
addr=$(<"./test/contracts/address1")

runScriptOnNode "./test/js/callSetSimpleStorage.js" 2 $addr 3

# prune and backup Node data
pruneAndBackupNode 0

# start node A, now using pruned DB
startNode 0 $bootAddress

#wait for node to resync
echo
echo "Waiting 30s for Node 0 to resync"
sleep 30 

# run a method call script on node 0, interacting with both old 
# and new SimpleStorage contracts
runScriptOnNode "./test/js/callSetSimpleStorage.js" 0 $addr 1

addr=$(<"./test/contracts/address0")
runScriptOnNode "./test/js/callSetSimpleStorage.js" 0 $addr 2

cleanUp
