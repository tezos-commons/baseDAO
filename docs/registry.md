<!--
SPDX-FileCopyrightText: 2021 Tezos Commons
SPDX-License-Identifier: LicenseRef-MIT-TC
-->

# Registry DAO

Registry DAO is a decentralized key-value storage of arbitrary data.
It's parameterized by two types: `k` (for key) and `v` (for value).
In the bundled in RegistryDAO, these `k` and `v` parameters are set as `string`s.

Extra storage data:
1. `big_map k v` is the actual key-value storage.
2. `big_map k proposalId` for each `k` stores IDs of the proposal that affected `k` last time.
3. `set address` is the set of successful-proposal receiver contract addresses.

We store these two `big_map`s separately because we want to include `proposalId` for deleted keys as well.
Alternatively, we can merge them and store `option v` values.
It should be treated as an implementation detail.
There is additional data to implement configuration callbacks, it's an implementation detail as well.

Proposals can be of different type and include (`proposal_metadata`) different data:

1. Proposals to update the parameters specified below taking
   - 5 `option nat` values:
      - `max_proposal_size`
      - `frozen_scale_value`
      - `frozen_extra_value`
      - `slash_scale_value`
      - `slash_division_value`
   - 2 `option tez` values
      - `min_xtz_amount`
      - `max_xtz_amount`
2. Proposal to update the set of successful-proposal receiver contract addresses.
This proposal takes a parameter that has two constructors. The proposal can add to,
or remove from the set of addresses.
3. Transfer proposals, that can transfer XTZ or tokens (FA2 as well as FA1.2) as well as update the
registry. This includes:
   - a list of `update` items parameterized by `k` and `v` types. Each `update` item contains a key of the type `k` and a new value of the type `option v`.
   - a list of transfer instructions. Each item in this list can be either of an Xtz transfer, an FA2 token transfer or an FA1.2 token transfer. These alternatives are represented by the type,
      ` (or (pair %xtz_transfer_type (mutez %amount) (address %recipient))
                                   (or (pair %token_transfer_type
                                          (address %contract_address)
                                          (list %transfer_list
                                             (pair (address %from_) (list %txs (pair (address %to_) (nat %token_id) (nat %amount))))))
                                       (pair %legacy_token_transfer_type
                                          (address %contract_address)
                                          (pair %transfer (address %from) (pair %target (address %to) (nat %value))))))`
   - `nat %agoraPostID` is used to refer to an Agora post explaining the proposed transfer and/or changes and motivation for them.
4. Proposal to update the guardian address in the BaseDAO contract:
   - This proposal takes an address parameter and use it to update the guardian address in the storage.
5. Proposal to update the contract delegate:
   - This proposal takes a `keyhash` `option` parameter and usees it to update the delegate address of the contract. Please note that this different from the `Update_delegate` entrypoint which updates the delegates of an account.

## Configuration callbacks

### Proposal check

The proposer must lock `frozen_scale_value * s + frozen_extra_value` tokens where `s = size(proposal_metadata)`.
I. e. `s` is total size of the diff and post ID.
It should naturally prohibit spam proposals and too big proposals (unless `frozen_scale_value` is 0).

Additionally, we require `s < max_proposal_size` as a safety measure because too large proposals can be too costly to deal with in terms of gas.

For transfer proposals, it's required that `min_xtz_amount` <= `amount` <= `max_xtz_amount`.
`max_proposal_size`, `frozen_scale_value`, `frozen_extra_value`, `min_xtz_amount` and `max_xtz_amount`
are parameters of the contract specified by the DAO creator.

Note that by setting `frozen_scale_value` to 0 it's possible to require a constant number of tokens to be locked.

### Rejected proposal slash amount

When a proposal is rejected, the amount of tokens to slash is computed as
`slash_scale_value * frozen / slash_division_value`.

`slash_scale_value` and `slash_division_value` are specified by the DAO creator.
One can set them to 1 and 1 by default to always slash all staked tokens.

### Decision callback

Except for the Guardian updates, It simply applies all updates from the accepted
proposal one by one.  They are applied in the same order as specified in the
proposal, the head of the list is applied first.

For the Update guardian proposal, it updates the guardian address in the storage.

# Proposal Check Errors

Here is a summary of all the strings used as error messages thrown by `proposal_check`.

| Error                                | Description                                                                                                 |
|--------------------------------------|-------------------------------------------------------------------------------------------------------------|
| `ZERO_MUTEZ`                         | Xtz transfer amount cannot be 0                                                                             |
| `LOW_XTZ`               | Xtz transfer amount cannot be smaller than 'min_xtz_amount'                                                 |
| `HIGH_XTZ`               | Xtz transfer amount cannot be bigger than 'max_xtz_amount'                                                  |
| `WRONG_TOKEN_AMOUNT`             | Incorrect token amounts locked                                                                              |
| `LARGE_PROPOSAL`                 | Proposal size is bigger than 'max_proposal_size'                                                            |

## Custom entrypoints

### Lookup registry on-chain view

A custom entrypoint `lookup_registry` is included in the registryDAO contract
to lookup values in the registry. The parameter of this entrypoint is `(pair k address)`
where `address` is the address of a callback contract of type `(k, v option)`.
