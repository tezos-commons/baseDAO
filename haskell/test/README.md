# BaseDAO LIGO tests

BaseDAO LIGO tests are implemented in Haskell, using the `cleveland` [library](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland).

## Running the test

To compile and execute them you'll need the `stack` tool, see
<!-- xrefcheck: ignore link -->[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial
for instructions on how to obtain it.

As always you can use the `Makefile` to run them, simply using:
```
make test
```

Alternatively you can also use the `stack test` command, but you'll need to
compile the contract and the example DAOs storages
(see [building instructions](../../docs/building.md)) and copy the resulting
files in the `haskell/resources` directory.
