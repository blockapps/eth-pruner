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
  PRIVATE_CONFIG=tm1.conf nohup geth --datadir qdata/dd1 $GLOBAL_ARGS --rpcport 22000 --port 21000 --unlock 0 --password passwords.txt 2>>qdata/logs/1.log &
  gethPIDs[0]="$!"

  echo "[*] Starting node 2"
  PRIVATE_CONFIG=tm2.conf nohup geth --datadir qdata/dd2 $GLOBAL_ARGS --rpcport 22001 --port 21001 --blockmakeraccount "0xca843569e3427144cead5e4d5999a3d0ccf92b8e" --blockmakerpassword "" --singleblockmaker --minblocktime 2 --maxblocktime 5 2>>qdata/logs/2.log &
  gethPIDs[1]="$!"

  echo "[*] Starting node 3"
  PRIVATE_CONFIG=tm3.conf nohup geth --datadir qdata/dd3 $GLOBAL_ARGS --rpcport 22002 --port 21002 --voteaccount "0x0fbdc686b912d7722dc86510934589e0aaf3b55a" --votepassword "" 2>>qdata/logs/3.log &
  gethPIDs[2]="$!"

  echo "[*] Starting node 4"
  PRIVATE_CONFIG=tm4.conf nohup geth --datadir qdata/dd4 $GLOBAL_ARGS --rpcport 22003 --port 21003 --voteaccount "0x9186eb3d20cbd1f5f992a950d808c4495153abd5" --votepassword "" 2>>qdata/logs/4.log &
  gethPIDs[3]="$!"

  echo "[*] Starting node 5"
  PRIVATE_CONFIG=tm5.conf nohup geth --datadir qdata/dd5 $GLOBAL_ARGS --rpcport 22004 --port 21004 --voteaccount "0x0638e1574728b6d862dd5d3a3e0942c3be47d996" --votepassword "" 2>>qdata/logs/5.log &
  gethPIDs[4]="$!"

  echo "[*] Waiting for nodes to start"
  sleep 10
}

function runScriptOnNode {
  echo
  echo "[*] Running script on Node $2"
  scriptPath=$1
  PRIVATE_CONFIG=tm$2.conf geth --exec "loadScript('$scriptPath')" attach ipc:qdata/dd$2/geth.ipc
}

function stopNode {
  echo
  echo "Stopping node: $1"
  kill -HUP ${gethPIDs[$1]}
}

function stopNodes {
  killall geth bootnode constellation-node
}

# initGethNodes

startConstellationNodes

startBootNode

startNodes

runScriptOnNode "createSimpleStorage.js" 1

sleep 5

runScriptOnNode "createPrivateStorage.js" 2

sleep 3
