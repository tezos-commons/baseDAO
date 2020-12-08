# baseDAO

BaseDAO is a basic generic smart contract for DAO on Tezos.

This repository provides:
* Generic DAO contract (template) that can be used as basis for various DAOs.
* Two instantiations of this template:
  1. Simplest implementation with no custom logic ([TrivialDAO contract](./src/Lorentz/Contracts/TrivialDAO.hs))
  2. A simple example with custom logic called [GameDAO](./src/Lorentz/Contracts/GameDAO.hs).
* A [template package](./template) that one can copy and use to create their own DAO.

It is implemented and tested using the [Morley framework](https://gitlab.com/morley-framework/morley)
and Haskell programming language.

## Contract documentation and requirements

The [specification](docs/specification.md) document is the specification used as a basis for the smart contract development.

Documentation of the actually implemented smart contract is generated automatically.
* [TrivialDAO](https://github.com/tqtezos/baseDAO/blob/autodoc/master/TrivialDAO.md)
* [GameDAO](https://github.com/tqtezos/baseDAO/blob/autodoc/master/GameDAO.md)

## Prerequisites

Further steps are given in assumption that `stack` tool is used to compile the project.
For how to obtain `stack` see [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial.

Also, you may need the very basic knowledge of Haskell and Lorentz, if this is your first time writing contracts on Lorentz, consider reading [the Lorentz documentation](https://gitlab.com/morley-framework/morley/-/blob/1fdefdb8c081235971cacc002b6704b709349d5c/code/lorentz/README.md).

## Build Instructions

You can use `stack build` to build `baseDAO` executable.

For how to obtain `stack` see [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial.

## Usage

Run `stack exec -- baseDAO --help` to see available commands.

### Deploying a contract

You can dump the entire contract code into a `BaseDAO.tz` file using the following command:

<!-- TODO: remove `--oneline` once morley:#442 is resolved -->

```sh
stack exec baseDAO -- print -n baseDAO --oneline
```

This will produce a contract with empty proposal metadata and the simplest possible configuration.
We also provide some other contracts as example, for instance `GameDAO` contract.

The initial storage of the contract can be produced using


```sh
admin=<administrator address>
stack exec baseDAO -- storage-BaseDAO --admin $admin > storage
```

Later the contract can be originated with `tezos-client`:

```sh
tezos-client originate contract baseDAO transferring 0 from alice running BaseDAO.tz --init "$(<storage)" --burn-cap 17.0
```

How to install `tezos-client` executable can be found at official [Tezos installation page](http://tezos.gitlab.io/introduction/howtoget.html) or in [our `tezos-packaging` repository](https://github.com/serokell/tezos-packaging).

### Documentation

You can obtain the actual version of documentation mentioned in [Contract documentation and requirements](#contract-documentation-and-requirements) for any specific contract via e.g.

```sh
stack exec baseDAO -- document -n BaseDAO
```

This command will dump the documentation to `BaseDAO.md` file.

## Writing your own DAOs

See [template DAO](./template) for instructions on how to write your own DAO contract.

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

Note that `stack build` won't work from `template` folder, because it serves as a copy-and-use package and contains its own `stack.yaml`.

## License

[MIT License](./LICENSE) Copyright (c) 2020 TQ Tezos
