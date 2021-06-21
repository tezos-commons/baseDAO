# BaseDAO

BaseDAO is a generic smart contract on Tezos that enables a community to collectively govern resources, registries, or rules.

The contract enables the creator to customize their DAO based on a number of attributes, and uses a ‘decision lambda’ to specify arbitrary code that can be governed by a DAO’s proposals.
BaseDAO also includes Permit (TZIP-17) to enable off-chain voting.

## Documentation and configurations

The [specification](docs/specification.md) document contains more details and is used as a basis for the smart contract development.

BaseDAO uses a runtime configuration, which is part of the overall storage of the
contract, meaning that DAOs with different logic and needs can be obtained by
providing a different initial storage.

Aside from the specification, that contains detail about the configuration as
well, there are also the [included DAOs](#included-daos) below.

## Getting/Building

BaseDAO is implemented in [LIGO](https://ligolang.org), you can obtain the
compiled contract as well as the initial storage for all the [included DAOs](#included-daos)
from the [latest release](https://github.com/tqtezos/baseDAO/releases/latest)
or build them from source by following the [building instructions](docs/building.md).

## Included DAOs

### [TrivialDAO](docs/trivial.md)

The simplest DAO, has no real logic configured but what's common to every DAO.
This has only demonstration purposes.

### [RegistryDAO](docs/registry.md)

Registry DAO is a decentralized key-value storage of arbitrary data.
Much like [Treasury DAO](#treasurydao) it can also hold XTZ and FA2 tokens that
its users can decide on how to spend.

### [TreasuryDAO](docs/treasury.md)

Treasury DAO is a DAO that holds XTZ and FA2 tokens and lets its users decide
how to spend its XTZ and tokens.

## Deploying/Originating

The contract can be originated with `tezos-client`:

```sh
tezos-client originate contract baseDAO transferring 0 from alice running BaseDAO.tz --init "$(<storage)" --burn-cap 17.0
```

Instructions on how to install `tezos-client` can be found at official [Tezos installation page](http://tezos.gitlab.io/introduction/howtoget.html) or in [our `tezos-packaging` repository](https://github.com/serokell/tezos-packaging).

## Typescript API

This contract includes a TypeScript API, based on [Taquito](https://tezostaquito.io/),
to interact with DAO contracts.

For more informations on its features and usage see [its documentation](typescript/baseDAO/README.md).

## Testing

Tests for this contract are implemented in Haskell using the [cleveland framework](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland).

For more detail about the test suite and how to execute it, check out its
[README](./haskell/test/README.md).

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## License

[MIT License](./LICENSES/LicenseRef-MIT-TQ.txt) Copyright (c) 2021 TQ Tezos
