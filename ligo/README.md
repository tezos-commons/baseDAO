# BaseDAO LIGO

__Note: This is currently in a very raw stage and can be originated only using
[this approach](https://gitlab.com/tezos/tezos/-/issues/1053#note_481537115).__

This is the LIGO implementation of BaseDAO, written in the cameLIGO dialect.

The main differences with its [Lorentz counterpart](/README.md) are:
- `BaseDAO Lorentz`:
    - uses compile-time configuration to define various options of a DAO,
    - as a consequence, each contract implementing over `baseDAO` will produce a different `Michelson` contract.
    - the configuration shapes the code before its generated, so the result is less flexible but more efficient

- `BaseDAO LIGO`:
    - uses runtime configuration, which is part of the overall storage of the contract
    - only one `Michelson` contract is used, the user simply defines the configuration as part of the storage at origination
    - the configuration is used by the code when it's executed, so the result is more flexible but not as efficient

## Build instructions

You will need the [ligo executable](https://ligolang.org/docs/intro/installation) installed to generate the contract.

You can use the [`Makefile`](./Makefile) to build the LIGO contract by simply running:
```sh
make all
```
which will save the compiled contract in `out/baseDAO.tz`.

## Generating `Storage`

Currently we don't provide any convenient way to generate the storage, so you'll
need to do so manually.

We will work on this in future versions.

## Using the contract

You will need [tezos-client](http://tezos.gitlab.io/introduction/howtoget.html) or similar software to originate and interact with the contract on the Tezos network.

For example, if you have compiled the contract and obtained a storage (`<storage>`) as described above you can originate it with:
`tezos-client originate contract myDAO transferring 1 from alice running out/baseDAO.tz --init <storage>`

## Testing

The tests for this contract are written in Haskell and you will need the `stack` tool to run them.

You can use the `Makefile` to do so as well:
```
make test
```
For more detail about LIGO tests, check out its [README.md](./haskell/test/)
