<!--
SPDX-FileCopyrightText: 2021 TQ Tezos
SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Building BaseDAO

BaseDAO is implemented using the [LIGO programming language](https://ligolang.org/),
in particular its CameLIGO dialect.

To generate the contract code, or the storage for one of the provided examples,
you will need the [ligo executable](https://ligolang.org/docs/intro/installation) installed.

The latest working version tested is [0.37.0](https://gitlab.com/ligolang/ligo/-/releases/0.37.0).

## Generating the contract code

Since this repo contain multiple variants, a slightly different command is required to build
the contract, depending on the variant that is required. For example, you can build the most simple
variant, the TrivialDAO LIGO contract using the [`Makefile`](../Makefile) in following command

```sh
make out/trivialDAO.tz
```
which will use `ligo compile contract` and save the result in `out/trivialDAO.tz`.

If you prefer to build it manually, the contract's main entrypoint is
`base_DAO_contract`, located in [src/base_DAO.mligo](../src/base_DAO.mligo).

## Generating the contract code for a custom variant

Imagine you want to implement a DAO called 'acme'. Variants can differ among each
other in a couple of ways.

1. Different type for the contract-extra field, and hence completely different storage type.
2. Different type for the custom entrypoint parameter, and hence completely different parameter type.
3. Different logic for various variant specific callbacks, like decision callback and proposal check.

To accomodate this, we need to create two ligo source files that wraps these differences. Specifically
these are the steps one should follow to accomplish this.

1. Create a new folder `src/variants/acme`.
2. Copy `src/variants/template/implementation.mligo` to `src/variants/acme/implementation.mligo`
   and `src/variants/template/storage.mligo` to `src/variants/acme/storage.mligo`.

Now you can follow the instructions in the comments for these files, to define the contract extra field
implement your logic for proposal check, decision callback and proposal check as well as the handler
for the custom entrypoint.

#### Defining a custom entrypoint type for the variant

For your custom variant, If you need to have two custom custom entrypoints of'my_custom_ep1' of type `(nat, nat)` and
'my_custom_ep2' of type `(int, int)` change the line:

```
type custom_ep_param = unit

```

to

```
type custom_ep_param =
  | My_custom_ep1 of (nat, nat)
  | My_custom_ep2 of (int, int)
```

if your custom entrypoints only contain a single entrypoint, you might have to pair
it with a dummy entrypoint so that the entrypoint annotation will show up in the final
contract.

```
type custom_ep_param =
  | MyCustomEp1 of (nat, nat)
  | MyCustomEpDummy of unit
```

#### Building the variant

After defining implementation.mligo and storage.mligo files for your variant, you can build it by using the make command:

```
make out/acmeDAO.tz
```

## Generating a contract metadata

BaseDAO metadata should be compliant with [TZIP-16](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md).

You can generate the known metadata, but you'll need the `stack` tool installed, see
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial for instructions on how to obtain it.

```bash
make metadata \
    frozen_token_symbol=frozen_token \
    frozen_token_name="BaseDAO Frozen Token" \
    frozen_token_thumbnail_uri="ipfs://QmV3a1TAdCncfs84Gi9msDsDJVQBDt6Wb5gJRVuFRfrgtG" \
    output=metadata.json
```
You can then modify the produced `metadata.json` if you want to add more information.

This metadata should be stored either:
- On IPFS: [Get started with IPFS](https://ipfs.io/)
- As a separate contract: [The `tezos-storage` URI Scheme](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#the-tezos-storage-uri-scheme)


To use this metadata, follow [TZIP-16 Contract storage](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#contract-storage)

Specifically:
> The encoding of the values must be the direct stream
of bytes of the data being stored. For instance, an URI starting with `http:`
will start with the 5 bytes `0x687474703a` (`h` is `0x68`, `t` is `0x74`,
etc.). There is no implicit conversion to Michelson's binary format (`PACK`) nor
quoting mechanism.

After the conversion, the bytes can be use in the argument `metadata_map` of the `make <storage>` command
(See the next section for more detail.)
```
make <storage.tz> \
  ... \
  metadata_map=(Big_map.literal [("", <metadata-bytes>)])
```

## Generating a contract storage

You can use `ligo` to also generate the contract initial storage, using the
`ligo compile-storage` command.

Following are the intructions to generate the initial storages of the DAO configuration
provided in this codebase, see the [README](../README.md).

### TrivialDAO

This storage is the one based on the [default storage values](../src/defaults.mligo).
```sh
make out/trivialDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
  guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
  governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
  governance_token_id=0n \
  start_level=100n \
  metadata_map=Big_map.empty \
  freeze_history=[] \
  fixed_proposal_fee_in_token=0n \
  quorum_threshold=10n \
  min_quorum=1n \
  max_quorum=99n \
  period=15840n  \
  quorum_change=5n \
  max_quorum_change=19n \
  governance_total_supply=1000n \
  proposal_flush_level=36000n  \
  proposal_expired_level=47520n \
```

The `admin_address`, `guardian_address`, `governance_token_address`,
`start_level` and `governance_token_id` are required values. The rest of the
arguments are optional and will be equal to the values above if not specified.

You can see the [specification](specification.md) for more info about these
values.

### RegistryDAO

This storage is defined in [src/variants/registry/implementation.mligo](../src/variants/registry/implementation.mligo), can be
compiled with `make`, as usual:
```sh
make out/registryDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
  guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
  governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
  governance_token_id=0n \
  frozen_scale_value=1n \
  frozen_extra_value=0n \
  max_proposal_size=100n \
  slash_scale_value=1n \
  slash_division_value=1n \
  min_xtz_amount=0mutez \
  max_xtz_amount=100mutez \
  start_level=100n \
  metadata_map=Big_map.empty \
  freeze_history=[] \
  fixed_proposal_fee_in_token=0n \
  quorum_threshold=10n \
  min_quorum=1n \
  max_quorum=99n \
  period=15840n \
  quorum_change=5n \
  max_quorum_change=19n \
  proposal_flush_level=36000n \
  proposal_expired_level= 47520n \
  governance_total_supply=1000n
```

The `admin_address`, `guardian_address`, `governance_token_address`,
`start_level`, and `governance_token_id` are required values. The rest of the
arguments are optional and will be equal to the values above if not specified.

### TreasuryDAO

This storage is defined in [src/variants/treasury/implementation.mligo](../src/variants/treasury/implementation.mligo) and
can be compiled once again with `make`:
```sh
make out/treasuryDAO_storage.tz \
  admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
  guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
  governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
  governance_token_id=0n \
  frozen_scale_value=1n \
  frozen_extra_value=0n \
  max_proposal_size=100n \
  slash_scale_value=1n \
  slash_division_value=1n \
  min_xtz_amount=0mutez \
  max_xtz_amount=100mutez \
  start_level=100n \
  metadata_map=Big_map.empty \
  freeze_history=Big_map.empty \
  fixed_proposal_fee_in_token=0n \
  freeze_history=[] \
  quorum_threshold=10n \
  min_quorum=1n \
  max_quorum=99n \
  period=15840n  \
  quorum_change=5n \
  max_quorum_change=19n \
  proposal_flush_level=36000n \
  proposal_expired_level=47520n \
  governance_total_supply=1000n
```

The `admin_address`, `guardian_address`, `governance_token_address`,
`start_level` and `governance_token_id` are required values. The rest of the
arguments are optional and will be equal to the values above if not specified.

## Storage generation checks
The LIGO functions used by the `Makefile` targets above perform some automatic check, specifically:
- if `freeze_history` is specified, then `total_supply` will be calculated, depending on its value
- `proposal_expired_time` will be rejected if it's not bigger than `proposal_flush_time`
- `proposal_flush_time` will be rejected if it's not bigger than twice the `period` length
- `quorum_threshold` will be rejected if bigger than `max_quorum` or smaller than `min_quorum`
