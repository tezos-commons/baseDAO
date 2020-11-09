# baseDAO

BaseDAO is a basic generic smart contracts for DAO on Tezos.
It is implemented and tested using the [Morley framework](https://gitlab.com/morley-framework/morley)
and Haskell programming language.

## Contract documentation and requirements

The [specification](docs/specification.md) document is the specification used as a basis for the smart contract development.

Documentation of the actually implemented smart contract is generated automatically.
* [BaseDAO](https://github.com/tqtezos/baseDAO/blob/autodoc/master/BaseDAO.md)
* [GameDAO](https://github.com/tqtezos/baseDAO/blob/autodoc/master/GameDAO.md)

## Build Instructions

You can use `stack build` to build `baseDAO` executable.

## Usage

Run `stack exec -- baseDAO --help` to see available commands.

### Deploying a contract

You can dump the entire contract code into a `BaseDAO.tz` file using the following command:

```sh
stack exec baseDAO -- print -n baseDAO
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

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## License

[MIT License](./LICENSE) Copyright (c) 2020 TQ Tezos
