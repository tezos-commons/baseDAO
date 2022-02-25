<!--
SPDX-FileCopyrightText: 2021 TQ Tezos
SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Trivial DAO

The simplest DAO, has no real logic configured but what's common to every DAO.
This has only demonstration purposes.

This DAO has no `extra` information associated to it.

## Configuration callbacks

### Proposal check

The proposal check is always successful, regardless of the content of its metadata
or the amount of `frozen_token`s.

### Rejected proposal slash amount

When a proposal is rejected, the amount to be slashed is a constant value of `0`.

In other words, the proposer will receive back all the staked tokens.

### Decision callback

This behaves like an identity:
- no change to the contract `extra` is performed
- no operation is issued

## Custom entrypoints

No custom entrypoints are defined.
