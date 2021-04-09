# BaseDAO

BaseDAO is a generic smart contract on Tezos that enables a community to collectively govern resources, registries, or rules.

The contract enables the creator to customize their DAO based on a number of attributes, and uses a ‘decision lambda’ to specify arbitrary code that can be governed by a DAO’s proposals.
BaseDAO also includes Permit (TZIP-17) to enable off-chain voting.

This repository provides:
* Generic DAO contract, written in cameLIGO, that can be used for various DAOs implementations.
* Instantiations of this contract:
  1. Simplest implementation with no custom logic ([TrivialDAO](#trivialdao))
  2. Two more practical DAO contracts: [RegistryDAO](#registrydao) and [TreasuryDAO](#treasurydao).
* A [TypeScript API](#typescript-api) to interact with DAO contracts.
* [Contract tests](#testing) to validate its implementation.

## Contract documentation and configurations

The [specification](docs/specification.md) document contains more details and is used as a basis for the smart contract development.

BaseDAO uses a runtime configuration, which is part of the overall storage of the
contract, meaning that DAOs with different logic and needs can be obtained by
providing a different initial storage.

Aside from the specification, that contains detail about the configuration as
well, there are also [example DAOs](#example-daos) below.

## Build instructions

You will need the [ligo executable](https://ligolang.org/docs/intro/installation) installed to generate the contract.

Latest working version tested is [0.10.0](https://gitlab.com/ligolang/ligo/-/releases/0.10.0).

You can use the [`Makefile`](./Makefile) to build the LIGO contract by simply running:
```sh
make out/baseDAO.tz
```
which will use `ligo compile-contract` and save the result in `out/baseDAO.tz`.

You will also need a contract initial storage, which can be obtained with the
`ligo compile-storage` command. You can also use:
```sh
make all
```
to generate the contract as well as all the [example DAOs](#example-daos)
defined below.

See the [`Makefile`](./Makefile) for details about these and the other commands.

## Originating the contract

This contract is too large to fit inside the current Tezos origination limit.

To overcome this limitation we advice to use the [morley-large-originator](https://gitlab.com/morley-framework/morley/-/tree/master/code/morley-large-originator)
tool or the workaround that it's described there.

Note: as explained in its README the latest version of its binary can be
downloaded from its repository's CI, without the need to build it.

The tool will give you the possibility to originate the contract for you, by performing
multiple operations, or produce the steps and instructions to let you do the
origination with your tool of choice (e.g. `tezos-client`).

As usual, both these actions can be performed with the help of the `Makefile`:
these are the usages with all the available arguments:
```sh
make originate \
  storage=out/trivialDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
  contract_name=baseDAO
```

```sh
make originate-steps \
  storage=out/trivialDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
  destination=out/steps
```

These configurable options are:
- the contract `storage` to use.
- the `admin_address` who'll perform the origination.
  Note: it doesn't have to be the same as the contract's `admin_address`.
  Note: in case of `originate-steps` this cannot be an alias, as that command
  makes no network operations and has no way of resolving it.
- the `contract_name` is the alias that the originated contract will be associated with.
- the `destination` is the directory where the steps files will be saved.

## Example DAOs

### TrivialDAO

The simplest DAO configuration is the one based on the
[default storage values](src/defaults.mligo).

This contract has only demonstration purposes and can be generated using:
```sh
make out/trivialDAO_storage.tz \
  admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  now_val=Tezos.now \
  metadata_map=(Big_map.empty : metadata_map)
```

All its arguments are optional and will be equal to the values above if not
specified.
You can see the [specification](docs/specification.md) for more info about these
values.

For a more interesting example you can instead use the [Registry DAO](#registrydao)
or the [Treasury DAO](#treasurydao) examples just below.

### RegistryDAO

Registry DAO is a decentralized key-value storage of arbitrary data.
Much like [Treasury DAO](#treasurydao) it can also hold XTZ and FA2 tokens that
its users can decide on how to spend.
See its [documentation](docs/registry.md) for more info.

The storage, defined in [src/registryDAO.mligo](src/registryDAO.mligo), can be
compiled with `make`, as usual:
```sh
make out/registryDAO_storage.tz \
  admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  max_proposal_size=100n \
  frozen_scale_value=1n \
  frozen_extra_value=0n \
  slash_scale_value=1n \
  slash_division_value=-n \
  min_xtz_amount=0mutez \
  max_xtz_amount=100mutez \
  now_val=Tezos.now \
  metadata_map=(Big_map.empty : metadata_map)
```

All its arguments are optional and will be equal to the values above if not
specified.

### TreasuryDAO

Treasury DAO is a DAO that holds XTZ and FA2 tokens and lets its users decide
how to spend its XTZ and tokens.
See its [documentation](docs/treasury.md) for more info.

The storage, defined in [src/treasuryDAO.mligo](src/treasuryDAO.mligo), can be
compiled once again with `make`:
```sh
make out/treasuryDAO_storage.tz \
  admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  max_proposal_size=12n \
  frozen_scale_value=1n \
  frozen_extra_value=0n \
  slash_scale_value=1n \
  slash_division_value=1n \
  min_xtz_amount=0mutez \
  max_xtz_amount=100mutez \
  now_val=Tezos.now \
  metadata_map=(Big_map.empty : metadata_map)
```

As for the other examples, all the arguments are optional and will be equal to
the values above if not specified.

## Typescript API

This contract includes a TypeScript API, based on [Taquito](https://tezostaquito.io/),
to interact with DAO contracts.

For more informations see on its features and usage see its [directory](./typescript).

## Testing

Tests for this contract are implemented in Haskell using the [cleveland framework](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland).

To compile and execute them you'll need the `stack` tool, see
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial
for instructions on how to obtain it.

As always you can use the `Makefile` to run them, simply using:
```
make test
```

For more detail about the test suite, check out its [README.md](./haskell/test/)

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## License

[MIT License](./LICENSES/LicenseRef-MIT-TQ.txt) Copyright (c) 2021 TQ Tezos
