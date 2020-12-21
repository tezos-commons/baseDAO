<!--
SPDX-FileCopyrightText: TQ Tezos

SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Registry DAO

Registry DAO is a decentralized key-value storage of arbitrary data.
It's parameterized by two types: `k` (for key) and `v` (for value).
In UI, these `k` and `v` parameters both will be `bytes` and will be converted to application-level types externally.
Application-level types can be stored as part of the contract's metadata as specified in TZIP-16.

Extra storage data:
1. `big_map k v` is the actual key-value storage.
2. `big_map k proposalId` for each `k` stores IDs of the proposal that affected `k` last time.

We store these two `big_map`s separately because we want to include `proposalId` for deleted keys as well.
Alternatively, we can merge them and store `option v` values.
It should be treated as an implementation detail.
There is additional data to implement configuration lambdas, it's an implementation detail as well.

Every proposal includes (`proposalMetadata`):
1. `nat %agoraPostID` is used to refer to an Agora post explaining the proposed changes and motivation for them.
2. A list of `update` items parameterized by `k` and `v` types. Each `update` item contains a key of the type `k` and a new value of the type `option v`.

Another type of proposal is used to update parameters specified below,
it takes 5 `option nat` values (called `s_max`, `a`, `b`, `c` and `d` below).

Configuration lambdas are implemented as follows:

* Proposal check: the proposer must lock `a * s + b` tokens where `s = size(pack(proposalMetadata))`.
I. e. `s` is total size of the diff and post ID.
It should naturally prohibit spam proposals and too big proposals (unless `a` is 0).
Additionally, we require `s < s_max` as a safety measure because too large proposals can be too costly to deal with in terms of gas.
`s_max`, `a` and `b` are parameters of the contract specified by the DAO creator.
Note that by setting `a` to 0 it's possible to require a constant of tokens to be locked.

* When a proposal is rejected, the returned amount is computed as `c * frozen / d`.
`c` and `d` are specified by the DAO creator.
One can set them to 1 and 1 by default to always unfreeze all tokens.

* The decision lambda simply applies all updates from the accepted proposal one by one.
They are applied in the same order as specified in the proposal, the head of the list is applied first.
