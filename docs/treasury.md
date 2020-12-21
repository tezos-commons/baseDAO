<!--
SPDX-FileCopyrightText: TQ Tezos

SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Treasury DAO

Treasury DAO is a smart contract that holds XTZ and FA2 tokens and lets its users decide how to spend its XTZ and tokens.
Its extra storage data is empty.

Every proposal (`proposalMetadata`) includes a list of items where each item contains:
1. `or %transfers (pair (mutez %amount) (address %recipient)) (pair (address %fa2) (list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))))` specifies what transfer to make. The left part is used for XTZ transfers, the right part is used for FA2 transfers.
2. `nat %agoraPostID` is used to refer to an Agora post explaining the proposed transfer and motivation for it.

Another type of proposal is used to update parameters specified below,
it takes 7 `option nat` values (called `s_max`, `a`, `b`, `c`, `d`, `y` and `z` below).

Configuration lambdas are implemented as follows:

* Proposal check: the proposer must lock `a * s + b` tokens where `s = size(pack(proposalMetadata))`.
I. e. `s` is total size of the transfers and post ID.
It should naturally prohibit spam proposals and too big proposals (unless `a` is 0).
Additionally, we require `s < s_max` as a safety measure because too large proposals can be too costly to deal with in terms of gas.
`s_max`, `a` and `b` are parameters of the contract specified by the DAO creator.
Note that by setting `a` to 0 it's possible to require a constant of tokens to be locked.
For XTZ transfers their amount must be in range `[y .. z]`.
`s_max`, `a`, `b`, `y` and `z` are parameters of the contract specified by the DAO creator.
Additionally, for XTZ transfers `CONTRACT unit` instruction must pass for the `recipient` address, i. e. this address must be an implicit account or refer to an entrypoint of the `unit` type.

* When a proposal is rejected, the returned amount is computed as `c * frozen / d` just like in Registry DAO.

* The decision lambda makes all requested transfers one by one. In case of insufficient balance the decision lambda can fail.
  + For XTZ transfers it sends `amount` to the `recepient` address with `unit` parameter. I. e. it returns a single `TRANSFER_TOKENS` operation.
  + For FA2 transfers it calls `transfer` entrypoint of the `fa2` contract with given argument.

Treasury DAO has explicit `default` entrypoint that is used to receive XTZ.
