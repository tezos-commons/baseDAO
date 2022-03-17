<!--
SPDX-FileCopyrightText: 2021 Tezos Commons
SPDX-License-Identifier: LicenseRef-MIT-TC
-->

# Treasury DAO

Treasury is a DAO that holds XTZ and FA2 tokens and lets its users decide how to
spend its XTZ and tokens. Its extra storage data is a unit value and thus contains
no data.

Its `proposal_metadata` contains proposal types:
- Transfer proposal that includes:
   - a list of items where each item contains:
   `or %transfers (pair (mutez %amount) (address %recipient)) (pair (address %fa2) (list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))))` specifies what transfer to make. The left part is used for XTZ transfers, the right part is used for FA2 transfers.
   - `nat %agoraPostID` is used to refer to an Agora post explaining the proposed transfer and motivation for it.
- Proposal to update the guardian address in the BaseDAO contract.
   - This proposal takes an address parameter and use it to update the guardian address in the storage.
- Proposal to update the contract delegate.
   - This proposal takes a `keyhash` `option` parameter and usees it to update the delegate address of the contract. Please note that this different from the `Update_delegate` entrypoint which updates the delegates of an account.


## Configuration callbacks

### Proposal check

The proposer must lock `frozen_scale_value * s + frozen_extra_value` tokens where `s = size(proposal_metadata)`.
I. e. `s` is total size of the transfers and post ID.
It should naturally prohibit spam proposals and too big proposals (unless `frozen_scale_value` is 0).

Additionally, we require `s < max_proposal_size` as a safety measure because too large proposals can be too costly to deal with in terms of gas.
`max_proposal_size`, `frozen_scale_value` and `frozen_extra_value` are parameters of the contract specified by the DAO creator.

Note that by setting `frozen_scale_value` to 0 it's possible to require a constant number of tokens to be locked.

For XTZ transfers their amount must be in range `[min_xtz_amount .. max_xtz_amount]`.
`max_proposal_size`, `frozen_scale_value`, `frozen_extra_value`, `min_xtz_amount` and `max_xtz_amount` are parameters of the contract specified by the DAO creator.
Additionally, for XTZ transfers `CONTRACT unit` instruction must pass for the `recipient` address, i. e. this address must be an implicit account or refer to an entrypoint of the `unit` type.

### Rejected proposal slash amount

When a proposal is rejected, the amount of tokens to slash is computed as
`slash_scale_value * frozen / slash_division_value`, just like in Registry DAO.

### Decision callback

For the Transfer proposal, it makes all requested transfers one by one.
 - In case of insufficient balance the decision callback can fail.
    + For XTZ transfers it sends `amount` to the `recepient` address with `unit` parameter. I. e. it returns a single `TRANSFER_TOKENS` operation.
    + For FA2 transfers it calls `transfer` entrypoint of the `fa2` contract with given argument.

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

Treasury DAO does not contain any custom entrypoints and its custom entrypoint type is unit.
