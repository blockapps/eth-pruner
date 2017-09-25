# eth-pruner
State Trie pruner for Ethereum state trie.

## Dependencies

The tools provided in eth-pruner require a functioning `geth`/`quorum` node to exist to be of any use. 

Those using these tools should be comfortable starting and stopping `geth`/`quorum` nodes.

## Building the source

Building eth-pruner requires [stack](https://docs.haskellstack.org/en/stable/README/).

Once the dependencies are installed, run

    stack install --ghc-options="-threaded -rtsopts"
    
The `--ghc-options="-threaded -rtsopts"` allows for using multiple cores if available on the machine.    

## Executables

The eth-pruner project comes with two executables.

| Command    | Description |
|:----------:|-------------|
| **`prune`** | The main pruner CLI. It will prune the levelDB powering a geth node from a selected block. It leaves the original database as is and constructing the pruned database in the folder `pruned_chaindata`. *Note:* You will need to manually move `pruned_chaindata` to `chaindata` to run the node using the pruned database.|
| `restore` | The restoration tool to restore a pruned levelDB using an un-pruned backup of the geth leveDB.|
| `count` | A useful tool to output the number of elements in levelDB by iterating through all the elements.|

## Running prune

Run `prune` in the directory containing the levelDB data, typically the levelDB data is stored in a folder called `chaindata`. `prune` takes 1 argument, the selected block number containing the stateroot that should be preserved. Make certain that the geth node is stopped. 

```
prune <BlockNumber> <InputDirectory> <OutputDirectory> +RTS -N2
```

The `+RTS -N2` options tell the pruner tool to make use of 2 cores. It will still work fine if the machine has only 1 core.

## Running restore

`restore` can be ran in any directory. `restore` takes 2 arguments: 

    1. the path to the directory of backup levelDB
    2. the path to the directory of the pruned levelDB
 
 Make certain that the geth node is stopped. 

```
restore <path/to/backup/levelDB> <path/to/pruned/levelDB>
```

## Running count

`count` can be ran in any directory. `count` takes 1 arguments, the path to the levelDB to count on. Make certain that the geth node is stopped.

```
count <path/to/levelDB>
```

### Warning

If geth is running when these tools are ran, there is a likely chance that data will be corrupted in the database, potentially rendering the geth node useless.

## Running the tests

There are 2 tests, a geth test and a quorum test. For the quorum test to run, it will need the correct binary of `geth` and the `constellation-node` binary. Make sure that `geth` and `bootnode` (and `constellation-node` for quorum) commands are installed as well as `node` version `6.11.1`. 

### Running geth test

```
cd test/geth
./test.sh
```


### Running quorum test

```
cd test/quorum
./init.sh
./test.sh
./stop.sh
```
