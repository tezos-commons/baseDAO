# BaseDAO LIGO tests

BaseDAO LIGO tests are implemented in Haskell, using `cleveland` [library](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland).

## Tests implementation

This directory contains:
- an [Haskell library](./src), containing Haskell binding for the contract
- a [Utility Executable](./app), mainly useful to produce metadata
- the [contract tests](./test), based on the library

## Running the test

To run the tests you'll need the [`stack`](https://haskellstack.org) tool installed.

You can execute them by simply calling:
```
make test
```

Alternatively you can also use the `stack test` command, but you'll need to
compile the contract and the example DAOs storages as well as setting these
environment variables needed by the testing suite:
- `BASEDAO_LIGO_PATH`: the path to the file with the baseDAO contract
- `REGISTRY_STORAGE_PATH`: the path to the file with the RegistryDAO initial storage
- `TREASURY_STORAGE_PATH`: the path to the file with the TreasuryDAO initial storage
