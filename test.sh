#!/bin/bash

a=`{
    "config": {
        "chainId": 15,
        "homesteadBlock": 0,
        "eip155Block": 0,
        "eip158Block": 0
    },
    "difficulty": "20000",
    "gasLimit": "2100000",
    "alloc": {
        "7df9a875a174b3bc565e6424a0050ebc1b2d1d82": { "balance": "300000" },
        "f41c74c9ae680c1aa78f42e5647a62f353b7bdde": { "balance": "400000" }
    }
}
`
echo $a
# initialize 3 db paths
# start bootstrap node
# get enode key from bootstrap node
# start 3 nodes with enode pubkey
# run a contracts creation script
# take node A down 
# run a contracts creation script on node B
# run a method call script on node B
# run pruner tool
# back up old data
# start node A back up with pruner DB
# run a method call script on node A
