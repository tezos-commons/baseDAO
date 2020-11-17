# baseDAO

BaseDAO is a basic generic smart contracts for DAO on Tezos.
It is implemented and tested using the [Morley framework](https://gitlab.com/morley-framework/morley)
and Haskell programming language.

## Contract documentation and requirements

The [specification](docs/specification.md) document is the specification used as a basis for the smart contract development.

Documentation of the actually implemented smart contract is generated automatically.
* [BaseDAO](https://github.com/tqtezos/baseDAO/blob/autodoc/master/BaseDAO.md)
* [GaseDAO](https://github.com/tqtezos/baseDAO/blob/autodoc/master/GameDAO.md)

## Build Instructions

You can use `stack build` to build `baseDAO` executable.

## Usage

Run `stack exec -- baseDAO --help` to see available commands.

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## License

[MIT License](./LICENSE) Copyright (c) 2020 TQ Tezos
