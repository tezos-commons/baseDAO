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

Currently we don't provide any convenient way to generate `BaseDAO` storage,
but you can use [RegistryDAO Storage generation](#registrydao) as example.

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

## Example contracts

### RegistryDAO

The storage, including configuration for the RegistryDAO [specification](https://github.com/tqtezos/baseDAO/blob/master/docs/registry.md)
is included in `ligo/src/registryDAO.mligo`. You can convert this into a
Michelson expression during the BaseDAO origination using the `ligo
compile-storage` command as follows.

```bash
ligo compile-storage ligo/src/registryDAO.mligo base_DAO_contract \
    'default_registry_DAO_full_storage(("tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" : address), ("tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" : address), 0n, 0n, 0n, 0n, 0n)'
```

The `default_registry_DAO_full_storage` is a LIGO function defined in
`ligo/src/registryDAO.mligo`, which returns a LIGO expression for storage,
which is converted into Michelson using the `compile-storage` command. The
arguments to this function are the admin address, the token address and the
configuration parameters described in the Registry spec, which are `a`, `b`,
`s_max`, `c` and `d`.

### TreasuryDAO

The storage, including configuration for the TreasuryDAO [specification](https://github.com/tqtezos/baseDAO/blob/master/docs/treasury.md)
is included in `ligo/src/treasuryDAO.mligo`. You can convert this into a
Michelson expression during the BaseDAO origination using the `ligo
compile-storage` command as follows.

```bash
ligo compile-storage ligo/src/treasuryDAO.mligo base_DAO_contract \
    'default_treasury_DAO_full_storage(("tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" : address), ("tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" : address), (0n, 0n, 0n, 0n, 0n, 0mutez, 100mutez))'
```

The `default_treasury_DAO_full_storage` is a LIGO function defined in
`ligo/src/treasuryDAO.mligo`, which returns a LIGO expression for storage,
which is converted into Michelson using the `compile-storage` command.

The arguments to this function are:
- the admin address
- the token address
- a tuple value consisting of configuration parameters described in the Treasury spec which are:
    - `a`, `b`, `s_max`, `c`, `d`, `x`, and `z`.
