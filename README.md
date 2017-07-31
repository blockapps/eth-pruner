# eth-pruner
State Trie pruner for Ethereum state trie.

## Building the source

Building eth-pruner requires [stack](https://docs.haskellstack.org/en/stable/README/).

Once the dependencies are installed, run

    stack install

## Executables

The eth-pruner project comes with two executables.

| Command    | Description |
|:----------:|-------------|
| **`prune`** | The main pruner CLI. It will prune the levelDB powering a geth node from a selected block. It leaves the original database as is and constructing the pruned database in the folder `pruned_chaindata`.|
| `count` | A useful tool to output the number of elements in levelDB by iterating through all the elements.|

## Running prune

Run `prune` in the directory containing the levelDB data, typically the levelDB data is stored in a folder called `chaindata`. `prune` takes 1 argument, the selected block number containing the stateroot that should be preserved. Make certain that the geth node is stopped. 

```
prune <block-number>
```

## Running count

Run `count` in the directory containing the levelDB data, typically the levelDB data is stored in a folder called `chaindata`. `count` takes 0 arguments. Make certain that the geth node is stopped.

```
count
```

### Warning

If geth is running when these tools are ran, there is a likely chance that data will be corrupted in the database that will render the geth node useless.

## Running the tests

To run the tests, make sure that `geth` and `bootnode` commands are installed as well as `node`. Simply run the command:

```
./test.sh
```
