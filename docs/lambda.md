<!--
SPDX-FileCopyrightText: 2021 Tezos Commons
SPDX-License-Identifier: LicenseRef-MIT-TC
-->

# Lambda DAO

LambdaDAO allows dynamic configuration of proposals and proposal handlers,
via proposals. This allows amending the logic of handling different kinds of
proposals, or even adding or removing proposal types.

LambdaDAO contains three concrete proposal types, which are "add handler", "remove handler"
and "execute handler". These are described below in detail.

Extra storage data:

1. `handler_storage` is the part of the storage that can be acted upon by the dynamic
proposal handlers.
2. `lambdas` is the field where all the dynamic proposal handlers are stored.

## Proposal types

1. Proposal to add a new proposal handler taking
    - `handler_name` which contains the name of the new proposal handler.
    - `code` which contains a lambda that implements the proposal logic.
    - `handler_check` which contains a lambda that checks the input of this new
       handler as part of the `proposal_check` procedure.

2. Proposal to disable a proposal handler taking
    - `string` which contains the name of the handler that should be disabled.

3. Proposal to execute a handler taking
    - `handler_name` which contains the name of the handler to execute.
    - `packed_argument` which contains the the arguments to this handler as a
    sequence of packed bytes.

## Configuration callbacks

### Proposal check

The proposer must lock `frozen_scale_value * s + frozen_extra_value` tokens
where `s = size(proposal_metadata)`. I. e. `s` is total size of the transfers
and post ID. It should naturally prohibit spam proposals and too big proposals
(unless `frozen_scale_value` is 0).

Additionally, we require `s < max_proposal_size` as a safety measure because
too large proposals can be too costly to deal with in terms of gas.
`max_proposal_size`, `frozen_scale_value` and `frozen_extra_value` are
parameters of the contract specified by the DAO creator.

Note that by setting `frozen_scale_value` to 0 it's possible to require a
constant number of tokens to be locked.

For XTZ transfers their amount must be in range `[min_xtz_amount ..
max_xtz_amount]`. `max_proposal_size`, `frozen_scale_value`,
`frozen_extra_value`, `min_xtz_amount` and `max_xtz_amount` are parameters of
the contract specified by the DAO creator. Additionally, for XTZ transfers
`CONTRACT unit` instruction must pass for the `recipient` address, i. e. this
address must be an implicit account or refer to an entrypoint of the `unit`
type.

In addition to this, there are separate proposal check logic for "add handler",
"remove_handler" and "execute_proposal" proposals, which are described below

#### add handler

The proposal check for the "add handler" proposal checks there are no proposal
handlers with the same name as the one being added.

#### remove handler

The proposal check for the "remove handler" proposal checks there is an active
proposal handler with the given name.

#### execute handler

The proposal check for the "execute handler" proposal checks there is an active
proposal handler with the given name.
It also fetches the "handler check" procedure for the handler, and runs it with
the input from this proposal.

### Rejected proposal slash amount

When a proposal is rejected, the amount of tokens to slash is computed as
`slash_scale_value * frozen / slash_division_value`, just like in Registry DAO.

### Decision callback

#### add handler

Adds the proposal handler to the storage. If the proposal handler exists, then
fails with `proposal_handler_exsits` error.

#### remove handler

Disables the proposal handler with the corresponding name from storage. The proposal
is not removed so that existing proposals that refer to this handler can proceed to
completion.

If an active handler with the given name was not found during flush, then this
proposal does nothing.

#### execute handler

Fetches the actual procedure associated with this handler and executes it passing the
input argument. It does not require that the handler is active.

# Proposal Check Errors

Here is a summary of all the strings used as error messages thrown by `proposal_check`.

| Error                                | Description                                                                                                 |
|--------------------------------------|-------------------------------------------------------------------------------------------------------------|
| `ZERO_MUTEZ`                         | Xtz transfer amount cannot be 0                                                                             |
| `LOW_XTZ`               | Xtz transfer amount cannot be smaller than 'min_xtz_amount'                                                 |
| `HIGH_XTZ`               | Xtz transfer amount cannot be bigger than 'max_xtz_amount'                                                  |
| `WRONG_TOKEN_AMOUNT`             | Incorrect token amounts locked                                                                              |
| `LARGE_PROPOSAL`                 | Proposal size is bigger than 'max_proposal_size'                                                            |
| `PROPOSAL_HANDLER_NOT_FOUND`                 | Proposal handler with a given name was not found in storage
| `PROPOSAL_HANDLER_EXISTS`                 | Proposal handler with a given name exists already
