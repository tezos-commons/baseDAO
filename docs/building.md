<!--
SPDX-FileCopyrightText: 2021 TQ Tezos
SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Building BaseDAO

BaseDAO is implemented using the [LIGO programming language](https://ligolang.org/),
in particular its CameLIGO dialect.

To generate the contract code, or the storage for one of the provided examples,
you will need the [ligo executable](https://ligolang.org/docs/intro/installation) installed.

The latest working version tested is [0.10.0](https://gitlab.com/ligolang/ligo/-/releases/0.10.0).

## Generating the contract code

You can use the [`Makefile`](../Makefile) to build the LIGO contract by simply running:
```sh
make out/baseDAO.tz
```
which will use `ligo compile-contract` and save the result in `out/baseDAO.tz`.

If you prefer to build it manually, the contract's main entrypoint is
`base_DAO_contract`, located in [src/base_DAO.mligo](../src/base_DAO.mligo).

You can also use:
```sh
make all
```
to generate the contract as well as all the example DAOs' initial storage
defined below.

## Generating a contract storage

You can use `ligo` to also generate a contract initial storage, using the
`ligo compile-storage` command.

Following are the intructions to generate the initial storages of the DAO configuration
provided in this codebase, see the [README](../README.md).

### TrivialDAO

This storage is the one based on the [default storage values](../src/defaults.mligo).
```sh
make out/trivialDAO_storage.tz \
  admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  governance_token_address="KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5" \
  governance_token_id=0n \
  now_val=Tezos.now \
  metadata_map=(Big_map.empty : metadata_map)
  fixed_proposal_fee_in_token=0n \
  ledger=([] : ledger_list) \
  quorum_threshold={numerator=1n; denominator=10n} \
  voting_period=950400n \
  metadata_map=(Big_map.empty : metadata_map) \
```

All its arguments are optional and will be equal to the values above if not
specified.

You can see the [specification](specification.md) for more info about these
values.

### RegistryDAO

This storage is defined in [src/registryDAO.mligo](../src/registryDAO.mligo), can be
compiled with `make`, as usual:
```sh
make out/registryDAO_storage.tz \
  admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  governance_token_address="KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5" \
  governance_token_id=0n \
  max_proposal_size=100n \
  frozen_scale_value=1n \
  frozen_extra_value=0n \
  slash_scale_value=1n \
  slash_division_value=-n \
  min_xtz_amount=0mutez \
  max_xtz_amount=100mutez \
  now_val=Tezos.now \
  metadata_map=(Big_map.empty : metadata_map) \
  fixed_proposal_fee_in_token=0n \
  ledger=([] : ledger_list) \
  quorum_threshold={numerator=1n; denominator=10n} \
  voting_period=950400n
```

All its arguments are optional and will be equal to the values above if not
specified.

### TreasuryDAO

This storage is defined in [src/treasuryDAO.mligo](../src/treasuryDAO.mligo) and
can be compiled once again with `make`:
```sh
make out/treasuryDAO_storage.tz \
  admin_address="tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af" \
  governance_token_address="KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5" \
  governance_token_id=0n \
  max_proposal_size=12n \
  frozen_scale_value=1n \
  frozen_extra_value=0n \
  slash_scale_value=1n \
  slash_division_value=1n \
  min_xtz_amount=0mutez \
  max_xtz_amount=100mutez \
  now_val=Tezos.now \
  metadata_map=(Big_map.empty : metadata_map)
  fixed_proposal_fee_in_token=0n \
  ledger=([] : ledger_list) \
  quorum_threshold={numerator=1n; denominator=10n} \
  voting_period=950400n \
  metadata_map=(Big_map.empty : metadata_map) \
```

As for the other examples, all the arguments are optional and will be equal to
the values above the values above if not specified.

## Storage generation checks
The LIGO functions used by the `Makefile` targets above perform some automatic check, specifically:
If `ledger` is specified, than `total_supply` will be calculated, depending on its value. \
The same is true for `voting_period`.
