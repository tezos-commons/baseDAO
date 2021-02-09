# BaseDAO LIGO

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

Defaults for the storage, including configuration, are included in the
`ligo/src/defaults.mligo` file.
This can be converted to a Michelson expression with the `ligo compile-storage`
command, which is included in the Makefile so it can be run using `make`.

```bash
make admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" out/trivialDAO_storage.tz
```

Note: Both arguments are optional and will be equal to the values above if not
specified.

Note also that `make all` will generate `out/trivialDAO_storage.tz` with these
default values as well, beside compiling the contract itself.

For a more interesting example you can use the
[RegistryDAO Storage generation](#registrydao) instead.

## Originating and starting up the contract

You will need [tezos-client](http://tezos.gitlab.io/introduction/howtoget.html) or similar software to originate and interact with the contract on the Tezos network.

For example, if you have compiled the contract and obtained a storage (`<storage>`) as described above you can originate it with:
`tezos-client originate contract myDAO transferring 1 from alice running out/baseDAO.tz --init <storage>`

Origination, however, won't be enough to make full use of the contract.
Due to the size limitations of Tezos, baseDAO will store its entrypoints as
lambdas inside its storage.

Because of this the contract starts in a "startup" mode and a series of operations
is necessary to load these entrypoint lambdas into the contract, before it can
be actually used.

To load (or override) an entrypoint into the storage, you can use:
`tezos-client transfer 0 from alice to myDAO --entrypoint 'startup' --arg '(Some (Pair "<ep_name>", (Some <ep_bytes>)))'`
which can also be used to load custom entrypoints.

For example, a simple custom entrypoint might look like:
`tezos-client transfer 0 from alice to myDAO --entrypoint 'startup' --arg '(Some (Pair "simple_ep" (Some 0x05020000000a03170316053d036d0342)))'`

All the standard entrypoint lambdas are provided in `ligo/src/startup.mligo`
(with some more information) and can be complied into parameters to push them to
the contract with the `ligo compile-parameter` command.

This is also included in the `Makefile` and using `make all` will provide each
parameter in the `out/entrypoints` directory, whose content is ready to be passed
as `--arg` to the command above.

As long as "startup" mode is going on only the `admin` can call a stored
entrypoint.

To remove a previously loaded entrypoint you can instead use
`tezos-client transfer 0 from alice to myDAO --entrypoint 'startup' --arg '(Some (Pair "<ep_name>", None))'`

Once all the entrypoints are loaded, you can exit the "startup" mode using:
`tezos-client transfer 0 from alice to myDAO --entrypoint 'startup' --arg 'None'`
IMPORTANT: this is irreversible, be careful when doing so.

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
is included in `ligo/src/registryDAO.mligo`.
As with the default storage this can be generated with the `ligo
compile-storage` command, or `make`:

```bash
make admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" s_max=12n a=1n b=0n c=1n d=1n out/registryDAO_storage.tz
```

All the arguments to the above command are optional, and will be filled with
following dummy addresses and default values.

```
admin_address = "tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af"
token_address = "tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af"
a = 1n
b = 0n
s_max = 100n
c = 1n
d = 0n
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
make admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" s_max=12n a=1n b=0n c=1n d=1n y=0mutez z=100mutez out/treasuryDAO_storage.tz
```

This uses the `default_treasury_DAO_full_storage` which is a LIGO function defined in
`ligo/src/treasuryDAO.mligo`, which returns a LIGO expression for storage,
which is converted into Michelson using the `compile-storage` command.

The arguments to this function are:
- the admin address
- the token address
- a tuple value consisting of configuration parameters described in the Treasury spec which are:
    - `a`, `b`, `s_max`, `c`, `d`, `x`, and `z`.
