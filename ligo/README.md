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
make admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" now_val=Tezos.now  metadata_map=(Big_map.empty : metadata_map) out/trivialDAO_storage.tz
```

Note: All arguments are optional and will be equal to the values above if not
specified.

Note also that `make all` will generate `out/trivialDAO_storage.tz` with these
default values as well, beside compiling the contract itself.

For a more interesting example you can use the
[RegistryDAO Storage generation](#registrydao) instead.

## Originating the contract

This contract is too large to fit inside the origination limit.

To overcome this limitation we advice to use the [morley-large-originator](https://gitlab.com/morley-framework/morley/-/tree/master/code/morley-large-originator)
tool or the workaround that it's described there.

Note: as explained in its README the latest version of its binary can just be
downloaded from its repository's CI, without the need to build it.

The tool will give you the possibility to originate the contract for you, by performing
multiple operations, or produce the steps and instructions to let you do the
origination with your tool of choice (e.g. `tezos-client`).

As usual, both these actions can be performed with the help of the `Makefile`:
For example, these are the usages with all the available arguments:
```
make originate storage=out/trivialDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
  contract_name=baseDAO

# or

make originate-steps storage=out/trivialDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af destination=out/steps
```

These configurable options are:
- the contract `storage` to use.
- the `admin_address` who'll perform the origination.
  Note: it doesn't have to be the same as the contract's `admin_address`.
  Note: in case of `originate-steps` this cannot be an alias, as that command
  makes no network operations and has no way of resolving it.
- the `contract_name` is the alias that the originated contract will be associated with.
- the `destination` is the directory where the steps files will be saved.

## Generating Typescript interface types

Typescript types representing the BaseDAO contract parameter can be generated automatically by using
the `make typescript` command. The files are saved to `../typescript/src/generated` directory.

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
make admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" max_proposal_size=12n frozen_scale_value=1n frozen_extra_value=0n slash_scale_value=1n slash_division_value=1n now_val=Tezos.now metadata_map=(Big_map.empty : metadata_map) out/registryDAO_storage.tz
```

All the arguments to the above command are optional, and will be filled with
following dummy addresses and default values.

```
admin_address = "tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af"
token_address = "tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af"
frozen_scale_value = 1n
frozen_extra_value = 0n
max_proposal_size = 100n
slash_scale_value = 1n
slash_division_value = 0n
min_xtz_amount = 0mutez
max_xtz_amount = 100mutez
now_val = Tezos.now
metadata_map = (Big_map.empty : metadata_map)
```

The `default_registry_DAO_full_storage` is a LIGO function defined in
`ligo/src/registryDAO.mligo`, which returns a LIGO expression for storage,
which is converted into Michelson using the `compile-storage` command. The
arguments to this function are the admin address, the token address and the
configuration parameters described in the Registry spec, which are
`frozen_scale_value`, `frozen-extra_value`, `max_proposal_size`,
`slash_scale_value`, `slash_division_value`, `min_xtz_amount` and `max_xtz_amount`.
Additionally, a `metadata` parameter is accepted, accepting a `big_map` as described in TZIP-16.

### TreasuryDAO

The storage, including configuration for the TreasuryDAO [specification](https://github.com/tqtezos/baseDAO/blob/master/docs/treasury.md)
is included in `ligo/src/treasuryDAO.mligo`. You can convert this into a
Michelson expression during the BaseDAO origination using the `ligo
compile-storage` command as follows.

```bash
make admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" token_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" max_proposal_size=12n frozen_scale_value=1n frozen_extra_value=0n slash_scale_value=1n slash_division_value=1n min_xtz_amount=0mutez max_xtz_amount=100mutez now_val=Tezos.now metadata_map=(Big_map.empty : metadata_map) out/treasuryDAO_storage.tz
```

This uses the `default_treasury_DAO_full_storage` which is a LIGO function defined in
`ligo/src/treasuryDAO.mligo`, which returns a LIGO expression for storage,
which is converted into Michelson using the `compile-storage` command.

The arguments to this function are:
- the admin address
- the token address
- a tuple value consisting of configuration parameters described in the Treasury spec which are:
    - `frozen_scale_value`,
    - `frozen_extra_value`,
    - `max_proposal_size`,
    - `slash_scale_value`,
    - `slash_division_value`,
    - `min_xtz_value`,
    - `max_xtz_value`.
- a `big_map` containing metadata as described in TZIP-16
