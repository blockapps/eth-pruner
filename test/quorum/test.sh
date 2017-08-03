#!/bin/bash
set -u
set -e
NETID=87234
BOOTNODE_KEYHEX=77bd02ffa26e3fb8f324bda24ae588066f1873d95680104de5bc2db9e7b2e510
BOOTNODE_ADDR=61077a284f5ba7607ab04f33cfde2750d659ad9af962516e159cf6ce708646066cd927a900944ce393b98b95c914e4d6c54b099f568342647a1cd4a262cc0423
BOOTNODE_ENODE=enode://61077a284f5ba7607ab04f33cfde2750d659ad9af962516e159cf6ce708646066cd927a900944ce393b98b95c914e4d6c54b099f568342647a1cd4a262cc0423@[127.0.0.1]:33445

GLOBAL_ARGS="--bootnodes $BOOTNODE_ENODE --networkid $NETID --rpc --rpcaddr 0.0.0.0 --rpcapi admin,db,eth,debug,miner,net,shh,txpool,personal,web3,quorum"

gethPIDs=()

function initGethNodes {

  echo "[*] Cleaning up temporary data directories"
  rm -rf qdata
  mkdir -p qdata/logs

  echo "[*] Configuring node 1"
  mkdir -p qdata/dd1/keystore
  cp keys/key1 qdata/dd1/keystore
  geth --datadir qdata/dd1 init genesis.json

  echo "[*] Configuring node 2 as block maker "
  mkdir -p qdata/dd2/keystore
  cp keys/key2 qdata/dd2/keystore
  geth --datadir qdata/dd2 init genesis.json

  echo "[*] Configuring node 3 as voter"
  mkdir -p qdata/dd3/keystore
  cp keys/key3 qdata/dd3/keystore
  geth --datadir qdata/dd3 init genesis.json

  echo "[*] Configuring node 4 as voter"
  mkdir -p qdata/dd4/keystore
  cp keys/key4 qdata/dd4/keystore
  geth --datadir qdata/dd4 init genesis.json

  echo "[*] Configuring node 5 as voter"
  mkdir -p qdata/dd5/keystore
  cp keys/key5 qdata/dd5/keystore
  geth --datadir qdata/dd5 init genesis.json

  sleep 3
}

function startConstellationNodes {
  echo "[*] Starting Constellation nodes"
  nohup constellation-node tm1.conf 2>> qdata/logs/constellation1.log &
  sleep 1
  nohup constellation-node tm2.conf 2>> qdata/logs/constellation2.log &
  nohup constellation-node tm3.conf 2>> qdata/logs/constellation3.log &
  nohup constellation-node tm4.conf 2>> qdata/logs/constellation4.log &
  nohup constellation-node tm5.conf 2>> qdata/logs/constellation5.log &
}

function startBootNode {
  echo "[*] Starting bootnode"
  nohup bootnode --nodekeyhex "$BOOTNODE_KEYHEX" --addr="127.0.0.1:33445" 2>>qdata/logs/bootnode.log &
  echo "wait for bootnode to start..."
  sleep 6
}

function startNodes {
  echo "[*] Starting node 1"
  PRIVATE_CONFIG=tm1.conf nohup geth --datadir qdata/dd1 $GLOBAL_ARGS --rpcport 22001 --port 21001 --unlock 0 --password passwords.txt 2>>qdata/logs/1.log &
  gethPIDs[0]="$!"

  echo "[*] Starting node 2"
  PRIVATE_CONFIG=tm2.conf nohup geth --datadir qdata/dd2 $GLOBAL_ARGS --rpcport 22002 --port 21002 --blockmakeraccount "0xca843569e3427144cead5e4d5999a3d0ccf92b8e" --blockmakerpassword "" --singleblockmaker --minblocktime 2 --maxblocktime 5 2>>qdata/logs/2.log &
  gethPIDs[1]="$!"

  echo "[*] Starting node 3"
  PRIVATE_CONFIG=tm3.conf nohup geth --datadir qdata/dd3 $GLOBAL_ARGS --rpcport 22003 --port 21003 --voteaccount "0x0fbdc686b912d7722dc86510934589e0aaf3b55a" --votepassword "" 2>>qdata/logs/3.log &
  gethPIDs[2]="$!"

  echo "[*] Starting node 4"
  PRIVATE_CONFIG=tm4.conf nohup geth --datadir qdata/dd4 $GLOBAL_ARGS --rpcport 22004 --port 21004 --voteaccount "0x9186eb3d20cbd1f5f992a950d808c4495153abd5" --votepassword "" 2>>qdata/logs/4.log &
  gethPIDs[3]="$!"

  echo "[*] Starting node 5"
  PRIVATE_CONFIG=tm5.conf nohup geth --datadir qdata/dd5 $GLOBAL_ARGS --rpcport 22005 --port 21005 --voteaccount "0x0638e1574728b6d862dd5d3a3e0942c3be47d996" --votepassword "" 2>>qdata/logs/5.log &
  gethPIDs[4]="$!"

  echo "[*] Waiting for nodes to start"
  sleep 10
}

function startNode {
  echo "[*] Starting node $1"
  ind=$1-1
  PRIVATE_CONFIG=tm$1.conf nohup geth --datadir qdata/dd$1 $GLOBAL_ARGS --rpcport 22001 --port 21001 --unlock 0 --password passwords.txt 2>>qdata/logs/$1.log &
  gethPIDs[$ind]="$!"
  echo "INDEX $ind"
}

function runScriptOnNode1 {
  echo
  echo "[*] Running script on Node $2"
  scriptPath=$1
  PRIVATE_CONFIG=tm$2.conf geth --exec "loadScript('$scriptPath')" attach ipc:qdata/dd$2/geth.ipc
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

function stopNodes {
  killall geth bootnode constellation-node
}

function pruneAndBackupNode {
  echo
  echo "Pruning and Backing Up node $1"

  dir="./qdata/dd$1/geth"
  blockNum=$(<"./selectedBlockNumber_$1")
  cd $dir
  countBefore=$(count "./chaindata")
  prune "$blockNum"
  mv chaindata chaindata_backup
  mv pruned_chaindata chaindata
  countAfter=$(count "./chaindata")
  echo "Item count before prune: $countBefore."
  echo "Item count after prune: $countAfter."
  cd ../../..
}

initGethNodes

node1PubKey=$(<"./keys/tm1.pub")
node2PubKey=$(<"./keys/tm2.pub")
node3PubKey=$(<"./keys/tm3.pub")
node4PubKey=$(<"./keys/tm4.pub")
node5PubKey=$(<"./keys/tm5.pub")

startConstellationNodes

startBootNode

startNodes

### CREATE 3 PUBLIC CONTRACTS, WITH PARTICIPANT NODE MAKING AT LEAST 1 OF THESE CONTRACTS

runScriptOnNode "createSimpleStorage.js" 1
runScriptOnNode "createSimpleStorage.js" 2
runScriptOnNode "createSimpleStorage.js" 3

### CREATE 3 PRIVATE CONTRACTS, 1 OF WHICH DOES NOT INTERACT WITH PARTICIPANT NODE

runScriptOnNode "createPrivateStorage.js" 1 $node1PubKey $node2PubKey $node5PubKey
runScriptOnNode "createPrivateStorage.js" 2 $node2PubKey $node1PubKey $node5PubKey
runScriptOnNode "createPrivateStorage.js" 5 $node5PubKey $node2PubKey $node4PubKey

### BEGIN
## ASSUMING SIMPLESTORAGE CONTRACTS, CHANGE THE STATE ON ALL CONTRACTS 3-4 TIMES USING SET FUNCTION

addr=$(<"./contracts/privateAddress1")
runScriptOnNode "callSetPrivateStorage.js" 1 $node1PubKey $addr 1 $node2PubKey $node5PubKey
runScriptOnNode "callSetPrivateStorage.js" 2 $node2PubKey $addr 2 $node1PubKey $node5PubKey
runScriptOnNode "callSetPrivateStorage.js" 5 $node5PubKey $addr 5 $node1PubKey $node2PubKey
runScriptOnNode "callSetPrivateStorage.js" 1 $node1PubKey $addr 6 $node2PubKey $node5PubKey

addr=$(<"./contracts/privateAddress2")
runScriptOnNode "callSetPrivateStorage.js" 2 $node2PubKey $addr 2 $node1PubKey $node5PubKey
runScriptOnNode "callSetPrivateStorage.js" 1 $node1PubKey $addr 1 $node2PubKey $node5PubKey
runScriptOnNode "callSetPrivateStorage.js" 5 $node5PubKey $addr 5 $node1PubKey $node2PubKey
runScriptOnNode "callSetPrivateStorage.js" 2 $node2PubKey $addr 6 $node1PubKey $node5PubKey

addr=$(<"./contracts/privateAddress5")
runScriptOnNode "callSetPrivateStorage.js" 5 $node5PubKey $addr 5 $node2PubKey $node4PubKey
runScriptOnNode "callSetPrivateStorage.js" 2 $node2PubKey $addr 2 $node4PubKey $node5PubKey
runScriptOnNode "callSetPrivateStorage.js" 4 $node4PubKey $addr 4 $node2PubKey $node5PubKey
runScriptOnNode "callSetPrivateStorage.js" 5 $node5PubKey $addr 6 $node4PubKey $node2PubKey

addr=$(<"./contracts/address1")
runScriptOnNode "callSetSimpleStorage.js" 1 $addr 1
runScriptOnNode "callSetSimpleStorage.js" 2 $addr 2
runScriptOnNode "callSetSimpleStorage.js" 3 $addr 3
runScriptOnNode "callSetSimpleStorage.js" 4 $addr 4

addr=$(<"./contracts/address2")
runScriptOnNode "callSetSimpleStorage.js" 2 $addr 2
runScriptOnNode "callSetSimpleStorage.js" 3 $addr 3
runScriptOnNode "callSetSimpleStorage.js" 4 $addr 4
runScriptOnNode "callSetSimpleStorage.js" 1 $addr 1

addr=$(<"./contracts/address3")
runScriptOnNode "callSetSimpleStorage.js" 3 $addr 3
runScriptOnNode "callSetSimpleStorage.js" 4 $addr 4
runScriptOnNode "callSetSimpleStorage.js" 1 $addr 1
runScriptOnNode "callSetSimpleStorage.js" 2 $addr 2

## END

# save the stateroot of the latest block to a file
runScriptOnNode "./selectBlock.js" 1 10

### STOP THE PARTICIPANT NODE

stopNode 1
pruneAndBackupNode 1

### CHANGE THE STATE OF 2 PUBLIC CONTRACTS

addr=$(<"./contracts/address1")
runScriptOnNode "callSetSimpleStorage.js" 2 $addr 2

addr=$(<"./contracts/address2")
runScriptOnNode "callSetSimpleStorage.js" 2 $addr 2

### CHANGE THE STATE OF ONE OF THE PRIVATE CONTRACTS INCLUDED WITH PARTICIPANT NODE

addr=$(<"./contracts/privateAddress1")
runScriptOnNode "callSetPrivateStorage.js" 2 $node2PubKey $addr 2 $node1PubKey $node5PubKey

### BRING PARTICIPANT NODE BACK UP

startNode 1

### VERIFY EXPECTED STATE OF ALL CONTRACTS

### CHANGE STATE OF ALL CONTRACTS (EXCEPT FOR EXCLUDED PRIVATE CONTRACT) USING PARTICIPANT NODE (NODE 1)

addr=$(<"./contracts/address1")
runScriptOnNode "callSetSimpleStorage.js" 1 $addr 1

addr=$(<"./contracts/address2")
runScriptOnNode "callSetSimpleStorage.js" 1 $addr 1

addr=$(<"./contracts/address3")
runScriptOnNode "callSetSimpleStorage.js" 1 $addr 1

addr=$(<"./contracts/privateAddress1")
runScriptOnNode "callSetPrivateStorage.js" 1 $node1PubKey $addr 1 $node2PubKey $node5PubKey

addr=$(<"./contracts/privateAddress2")
runScriptOnNode "callSetPrivateStorage.js" 1 $node1PubKey $addr 1 $node2PubKey $node5PubKey
