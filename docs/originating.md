<!--
SPDX-FileCopyrightText: 2021 TQ Tezos
SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Originating BaseDAO

Due to the size limitation of origination operations of Tezos, this contract
cannot be deployed to the network in the standard manner.

[morley-large-originator](https://gitlab.com/morley-framework/morley/-/tree/master/code/morley-large-originator)
is a tool developed exactly to overcome this issue.

Alternatively, you can use the workaround that the tool is based on, which is
described in its documentation.

## Using morley-large-originator

You can build `morley-large-originator` following the instructions in its
[documentation](https://gitlab.com/morley-framework/morley/-/tree/master/code/morley-large-originator) or you can also download the binary for it from the
[latest release](https://github.com/tqtezos/baseDAO/releases/latest)
of this repository.

The tool will give you the possibility to originate the contract for you, by
performing multiple operations, or produce the steps and instructions to let you
do the origination with your tool of choice (e.g. `tezos-client`).

Both of these actions, as well as many more, can be performed with the help of the
[Makefile](../Makefile).
These are the usages with all the available arguments:
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
