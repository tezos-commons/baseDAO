<!--
SPDX-FileCopyrightText: 2021 TQ Tezos
SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Table of contents

* [Overview](#overview)
* [General requirements](#general-requirements)
* [Configuration](#configuration)
* [Contract logic](#contract-logic)
* [Errors](#errors)
* [Entrypoints](#entrypoints)
* [TZIP-016 metadata](#tzip-016-metadata)

# Overview

The contract described here consists of two parts:
- Token functionality ([FA2-based][FA2]).
- DAO functionality with proposals and voting.

These two parts are coupled into one smart contract to reduce interactions
between smart contracts, which is expensive and hard to get right in Tezos.
However, an existing separate FA2 contract is used for governance.

# General Requirements

- The contract must be FA2 compatible.

- The contract must store frozen tokens (`token_id` is 0).

- The contract may store tokens with other token identifiers.

- The contract must store a 'governance token'. This consist of the address of
  an FA2 contract and a token id in that contract. The 'governance token'  is
  used as part of the freeze/unfreeze process by making FA2 transfers on it.

- The storage of the contract must have annotations for all fields
  and must be documented to make its interpretation easy for users.

# Configuration

BaseDAO is a concrete smart contract, but also a framework to implement various DAOs.
It can be configured at origination for any specific needs.

In order to do so the contract has types that can contain arbitary data:
- `proposal_metadata` which is a type synonym for `bytes`
- `contract_extra` which is a type synonym for `(string, bytes) big_map`
  (or in Michelson: `big_map string bytes`)


The former contains fields that are required to submit a proposal.

The latter keeps global information like information about accepted proposals.
In this case, we associate a `string` "name" to the `pack`ed representation of
the data, that can then be `unpack`ed by the contract code.

For example, the `proposal_metadata` in a "treasury" style DAO would be the packed version of the type `transfer_proposal` :
```ocaml
type xtz_transfer =
  { amount : tez
  ; recipient : address
  }

type token_transfer =
  { contract_address : address
  ; transfer_list : transfer_item list
  }

type transfer_type =
  | Xtz_transfer_type of xtz_transfer
  | Token_transfer_type of token_transfer

type transfer_proposal =
  { agora_post_id : nat
  ; transfers : transfer_type list
  }
```
and an empty `contract_extra`.

Lastly, there is one more `(string, bytes) big_map` type synonym: `custom_entrypoints`,
used to execute arbitrary logic on the contract, see [its section](#custom-entrypoints)
for more information on its content and usage.

DAO configuration value parameters are captured by the `config` type:

```ocaml
type config =
  { proposal_check : propose_params * storage -> bool
  // ^ A lambda used to verify whether a proposal can be submitted.
  // It checks 2 things: the proposal itself and the amount of tokens frozen upon submission.
  // It allows the DAO to reject a proposal by arbitrary logic and captures bond requirements
  ; rejected_proposal_return_value : proposal * storage -> nat
  // ^ When a proposal is rejected, the value that voters get back can be slashed.
  // This lambda returns the amount to be slashed.
  ; decision_lambda : proposal * storage -> operation list * storage
  // ^ The decision lambda is executed based on a successful proposal.
  // It has access to the proposal, can modify `contractExtra` and perform arbitrary
  // operations.

  ; max_proposals : nat
  // ^ Determine the maximum number of ongoing proposals that are allowed in the contract.
  ; max_votes : nat
  // ^ Determine the maximum number of votes associated with a proposal including positive votes
  ; max_quorum_threshold : quorum_threshold
  // ^ Determine the maximum value of quorum threshold that is allowed to be set.
  ; min_quorum_threshold : quorum_threshold
  // ^ Determine the minimum value of quorum threshold that is allowed to be set.
  ; max_voting_period : nat
  // ^ Determine the maximum value of voting period that is allowed to be set.
  ; min_voting_period : nat
  // ^ Determine the minimum value of voting period that is allowed to be set.

  ; custom_entrypoints : custom_entrypoints
  // ^ Packed arbitrary lambdas associated to a name for custom execution.
```

Note:
- the `token_metadata` type matches the one defined in FA2.
- the `proposal` type is defined below.
- the `quorum_threshold` is expressed as a `nat/nat` fraction of the total supply
  of frozen tokens, see [set_quorum_threshold](#set_quorum_threshold).
- `storage` is the storage type of the contract without the configuration.
- `full_storage` is instead the full storage of the contract, including its configuration,
which is to say: `type full_storage = storage * config`.


```ocaml
type proposal_key = bytes

type proposal =
  { upvotes : nat
  ; downvotes : nat
  ; start_date : timestamp
  ; metadata : proposal_metadata
  ; proposer : address
  ; proposer_frozen_token : nat
  ; proposer_fixed_fee_in_token : nat
  // ^ A fee paid for submitting the proposal.
  // Needed to refund correctly if the proposal is successful.
  ; voters : (address * nat) list
  // ^ List of voter addresses associated with the vote amount
  // Needed for `flush` entrypoint.
  }
```

## Runtime configuration

Some configuration values are specified in runtime and can be changed during the contract lifetime.
They must be provided on origination to construct the contract's initial storage.
These values are:
1. `admin : address` is the address that can perform administrative actions.
2. `governance_token` is the FA2 contract address/token id pair that will be used as the governance token.
3. `voting_period : nat` specifies how long the voting period lasts.
4. `quorum_threshold : quorum_threshold` specifies what fraction of the frozen
   tokens total supply of total votes are required for a successful proposal.
5. `fixed_proposal_fee_in_token : nat` specifies the fee for submitting a proposal (in native DAO token).

# Contract logic

This chapter provides a high-level overview of the contract's logic.

- The contract maintains a ledger of address and its balance (frozen tokens and optionally others).
- The contract maintains the address of an a FA2 contract and a token id, to use in the governance process.
- The contract manages a special role called "Administrator".
- The contract stores a list of proposals that can be in one of the states: "ongoing", "rejected", or "accepted".
- The contract forbids transferring XTZ to the contract, because they will be locked forever.

## Roles

The token supports one "global" user role: `Administrator`. This role applies to the
whole contract (hence "global"):

* **administrator**
  - Can re-assign this role to a new address.
  - Can perform administrative operations.
  - Can transfer FA2 tokens owned or operated by this contract.
  - There always must be exactly one administrator.

Additionally, the contract inherits the **operator** role from FA2.
This role is "local" to a particular address.
Each address can have any number of operators and be an operator of any number of addresses.

## Stages

The contract constantly cycles between two stages, a proposing stage and a voting stage.
Both have the same same length, `voting_period` and alternate between each other,
starting from "voting" for period number `0`.

Tokens can be frozen in any period, but they can only be used for voting, proposing
and unfreezing starting from the following one and onwards.

For freezing, the address should have corresponding amount of tokens of proper
token type (token_id of storage.governance_token.token_id) in the governance
FA2 contract. During 'Freeze' operation the BaseDAO contracts makes an FA2
transfer on the governance contract to transfer tokens to its own address from
the address who is freezing the tokens. Then it mints frozen tokens for the address.

Unfreezing does it reverse, that is, the contract makes the FA2 transfer on governance
contract to transfer tokens from its own address to the address that is doing unfreezing.
It then burns the corresponding amount of tokens. Only frozen tokens that are not currently
staked in a vote or proposal can be unfreezed.

### Proposals

Everyone can make a new proposal, however, you have to freeze some tokens for that.
The proposer specifies how many frozen tokens they want to stake and this value is checked by
the contract according to its compile-time configuration.

Proposing can only be performed in a proposing stage period, meaning one that's odd-numbered and
the proposer must have frozen his tokens in one of the preceding periods.

Proposals are identified by a key which is a `bytes` value computed via Blake2B hashing function of a
pair of propose entrypoint params and the proposer address.

### Voting

Once a proposal is submitted, everyone can vote on it as long as they have enough frozen tokens to stake.
One frozen token is required for one vote.

A vote can only be cast in a voting stage period, meaning one that's even-numbered.
Moreover the proposal to vote on must have been submitted in the proposing period immediately preceding
and the voter must have frozen his tokens in one of the preceding periods.
Voting period is specified for the whole smart contract and can be updated by the administrator; on update, the existing proposals are also affected.
It's possible to vote positively or negatively.
After the voting ends, the contract is "flushed" by calling a dedicated entrypoint.

## Ledger

Every address that is stored in ledger is associated with its balance associated with the frozen
token id.

# Errors

In error scenarios the baseDAO contract fails with a string or a pair where the first item is a string.
Here is a summary of all the strings used as error messages.
We start with standard FA2 errors which are part of the FA2 specification.

| Error                      | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `FA2_TOKEN_UNDEFINED`      | One of the specified `token_id`s is not defined                             |
| `FA2_INSUFFICIENT_BALANCE` | Cannot debit from a wallet because of excessive amount of tokens            |
| `FA2_NOT_OPERATOR`         | A transfer is initiated neither by the token owner nor a permitted operator |

The next group consists of the errors that are not part of the FA2 specification.
The list of errors may be inaccurate and incomplete, it will be updated during the implementation.

| Error                           | Description                                                                                                 |
|---------------------------------|-------------------------------------------------------------------------------------------------------------|
| `NOT_ADMIN`                     | The sender is not the administrator                                                                         |
| `NOT_PENDING_ADMIN`             | Authorized sender is not the current pending administrator                                                  |
| `NOT_TOKEN_OWNER`               | Trying to configure operators for a different wallet which sender does not own                              |
| `FAIL_PROPOSAL_CHECK`           | Throws when trying to propose a proposal that does not pass `proposalCheck`                                 |
| `FROZEN_TOKEN_NOT_TRANSFERABLE` | Transfer entrypoint is called for frozen token by a non-admin sender                                        |
| `PROPOSAL_NOT_EXIST`            | Throws when trying to vote on a proposal that does not exist                                                |
| `QUORUM_NOT_MET`                | A proposal is flushed, but there are not enough votes                                                       |
| `VOTING_PERIOD_OVER`            | Throws when trying to vote on a proposal that is already ended                                              |
| `OUT_OF_BOUND_VOTING_PERIOD`    | Throws when trying to set voting period that is out of bound from what is specified in the `config`         |
| `OUT_OF_BOUND_QUORUM_THRESHOLD` | Throws when trying to set quorum threshold that is out of bound from what is specified in the `config`      |
| `MAX_PROPOSALS_REACHED`         | Throws when trying to propose a proposal when proposals max amount is already reached                       |
| `MAX_VOTES_REACHED`             | Throws when trying to vote on a proposal when the votes max amount of that proposal is already reached      |
| `FORBIDDEN_XTZ`                 | Throws when some XTZ was received as part of the contract call                                              |
| `PROPOSER_NOT_EXIST_IN_LEDGER`  | Expect a proposer address to exist in Ledger but it is not found                                            |
| `PROPOSAL_NOT_UNIQUE`           | Trying to propose a proposal that is already existed in the Storage.                                        |
| `MISSIGNED`                     | Parameter signature does not match the expected one - for permits.                                          |
| `ENTRYPOINT_NOT_FOUND`          | Throw when `CallCustom` is called with a non-existing entrypoint                                            |
| `UNPACKING_FAILED`              | Throw when unpacking of a stored entrypoint, its parameter or a required `extra` value fails.               |
| `MISSING_VALUE`                 | Throw when trying to unpack a field that does not exist.                                                    |
| `NOT_PROPOSING_PERIOD`          | Throw when `propose` call is made on a non-proposing period.                                                |
| `NOT_ENOUGH_FROZEN_TOKENS`      | Throw when there is not enough frozen tokens for the operation.                                             |
| `NOT_ENOUGH_STAKED_TOKENS`      | Throw when there is not enough staked tokens for the operation.                                             |
| `BAD_TOKEN_CONTRACT`            | Throw when the token contract is not of expected type.

# Entrypoints

Full list:
* [`transfer`](#transfer)
* [`balance_of`](#balance_of)
* [`update_operators`](#update_operators)
* [`mint`](#mint)
* [`burn`](#burn)
* [`transfer_contract_tokens`](#transfer_contract_tokens)
* [`transfer_ownership`](#transfer_ownership)
* [`accept_ownership`](#accept_ownership)
* [`propose`](#propose)
* [`set_fixed_fee_in_token`](#set_fixed_fee_in_token)
* [`set_voting_period`](#set_voting_period)
* [`set_quorum_threshold`](#set_quorum_threshold)
* [`vote`](#vote)
* [`flush`](#flush)
* [`drop_proposal`](#drop_proposal)
* [`get_vote_permit_counter`](#get_vote_permit_counter)
* [`get_total_supply`](#get_total_supply)
* [`freeze`](#freeze)
* [`unfreeze`](#unfreeze)

Format:
```
**entrypoint_name**

<optional CameLIGO definition of the argument type>
Parameter (in Michelson): X

<description>
```

* Top-level contract parameter type MUST have all entrypoints listed below.
* Each entrypoint MUST be callable using the standard entrypoints machinery of Michelson by specifying **entrypoint_name** and a value of the type `X` (its argument).
* The previous bullet point implies that each `X` must have a field annotations with the corresponding entrypoint name.
In the definitions below it may be omitted, but it is still implied.

Note: CameLIGO definitions are provided only for readability.
If Michelson type contradicts what's written in CameLIGO definition, the Michelson definition takes precedence.

## Standard FA2 Token Functions

Functions present in the [*FA2 Tezos Token Standard*][FA2].

### **transfer**

```ocaml
type token_id = nat

type transfer_destination =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }

type transfer_item =
  [@layout:comb]
  { from_ : address
  ; txs : transfer_destination list
  }

type transfer_params = transfer_item list

Transfer of transfer_params
```

Parameter (in Michelson):
```
(list %transfer
  (pair
    (address %from_)
    (list %txs
      (pair
        (address %to_)
        (pair
          (nat %token_id)
          (nat %amount)
        )
      )
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

- Permission logic follows the default permissions descriptor specified in FA2.

- The administrator can transfer frozen tokens from any address to any address.

### **balance_of**

```ocaml
type token_id = nat

type balance_request_item =
  [@layout:comb]
  { owner : address
  ; token_id : token_id
  }

type balance_response_item =
  [@layout:comb]
  { request : balance_request_item
  ; balance : nat
  }

type balance_request_params =
  [@layout:comb]
  { requests : balance_request_item list
  ; callback : balance_response_item list contract
  }

Balance_of of balance_request_params
```

Parameter (in Michelson):
```
(pair %balance_of
  (list %requests
    (pair
      (address %owner)
      (nat %token_id)
    )
  )
  (contract %callback
    (list
      (pair
        (pair %request
          (address %owner)
          (nat %token_id)
        )
        (nat %balance)
      )
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

### **update_operators**

```ocaml
type token_id = nat

type operator_param =
  [@layout:comb]
  { owner : address
  ; operator : address
  ; token_id : token_id
  }
type update_operator =
  [@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param

type update_operators_param = update_operator list

Update_operators of update_operators_param
```

Parameter (in Michelson)
```
(list %update_operators
  (or
    (pair %add_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id)
      )
    )
    (pair %remove_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id)
      )
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

- Each `owner` must be equal to `SENDER`, otherwise `NOT_OWNER` error occurs.

- The `token_id` field must not be equal to frozen token identifier (zero), otherwise `OPERATION_PROHIBITED` error occurs.

## Custom (non-FA2) token functions

Functions related to token transfers, but not present in FA2.

### **mint**

```ocaml
type mint_param =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }

Mint of mint_param
```

Parameter (in Michelson):
```
(pair %mint
  (address %to_)
  (pair
    (nat %token_id)
    (nat %amount)
  )
)
```

- Produces the given amounts of tokens to the wallet associated with the given address.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `FA2_TOKEN_UNDEFINED` if trying to mint an unsupported token.

### **burn**

```ocaml
type burn_param =
  [@layout:comb]
  { from_ : address
  ; token_id : token_id
  ; amount : nat
  }

Burn of burn_param
```

Parameter (in Michelson):
```
(pair %burn
  (address %from_)
  (pair
    (nat %token_id)
    (nat %amount)
  )
)
```

- Reduce the given amounts of tokens to the wallet associated with the given address.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `FA2_INSUFFICIENT_BALANCE` if the wallet associated with the given address
does not have enough tokens to burn.

### **transfer_contract_tokens**

```ocaml
type token_id = nat

type transfer_destination =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }

type transfer_item =
  [@layout:comb]
  { from_ : address
  ; txs : transfer_destination list
  }

type transfer_params = transfer_item list

type transfer_contract_tokens_param =
  { contract_address : address
  ; params : transfer_params
  }

Transfer_contract_tokens of transfer_contract_tokens_param
```

Parameter (in Michelson):
```
(pair %transfer_contract_tokens
  (address %contract_address)
  (list %params
    (pair (address %from_)
      (list %txs
        (pair
          (address %to_)
          (pair
            (nat %token_id)
            (nat %amount)
          )
        )
      )
    )
  )
)
```

- This entrypoint can be used by the administrator to transfer tokens owned (or operated) by this contract in another FA2 contract.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- If the outermost address passed to this entrypoint is a smart contract with FA2 `transfer` entrypoint, this entrypoint is called with supplied argument.
That is, the list of operations returned from the baseDAO contract should contain one `TRANSFER_TOKENS` operation calling the `transfer` entrypoint.
Otherwise the call fails.

## Role reassigning functions

### **transfer_ownership**

```ocaml
type transfer_ownership_param = address

Transfer_ownership of transfer_ownership_param
```

Parameter (in Michelson):
```
(address %transfer_ownership)
```

- Initiate transfer of the role of administrator to a new address.

- Fails with `NOT_ADMIN` if the sender is not the administrator.

- The current administrator retains his privileges up until
  `accept_ownership` is called.

- Can be called multiple times, each call replaces pending administrator with
  the new one. Note, that if proposed administrator is the same as the current
  one, then the pending administrator is simply invalidated.

### **accept_ownership**

```ocaml
Accept_ownership of unit
```

Parameter (in Michelson):
```
(unit %accept_ownership)
```

- Accept the administrator privilege.

- Fails with `NOT_PENDING_ADMIN` if the sender is not the current pending administrator, this also includes the case when pending administrator was not set.

- When pending administrator is not set, it is considered equal to the current owner, thus administrator can accept ownership of its own contract without a prior `transfer_ownership` call.

## Proposal entrypoints

### **propose**

```ocaml
type proposal_metadata = bytes

type propose_params =
  { frozen_token : nat
  ; proposal_metadata : proposal_metadata
  }

Propose of propose_params
```

Parameter (in Michelson):
```
(pair %propose
  (nat %frozen_token)
  (bytes %proposal_metadata)
)
```

- The proposal is saved under `BLAKE2b` hash of proposal value and sender.
- The `Natural` value: `proposalTokenAmount` determines how many sender's frozen tokens will be staked in addition to the [fee](#set_fixed_fee_in_token).
- Sender MUST have enough frozen tokens (i. e. `≥ proposalTokenAmount + fee`) that are not already staked for a proposal or a vote.
- Fails with `NOT_ENOUGH_FROZEN_TOKENS` if the unstaked frozen token balance of the SENDER
  is less than `proposalTokenAmount + fee`.
- Fails with `NOT_PROPOSING_PERIOD` if the current period is not a proposing stage.
- Fails with `FAIL_PROPOSAL_CHECK` if the proposal is rejected by `proposalCheck` from the configuration.
- Fails with `MAX_PROPOSALS_REACHED` if the current amount of ongoing proposals is at max value set by the config.
- Fails with `PROPOSAL_NOT_UNIQUE` if exactly the same proposal from the same author has been proposed.

### **set_fixed_fee_in_token**

```ocaml
Set_fixed_fee_in_token of nat
```

Parameter (in Michelson):
```
(nat %set_fixed_fee_in_token)
```

- Update the fee that the proposers have to pay to submit a proposal (the fee is returned if the proposal is successful and burnt otherwise)
- This affects only new proposals; the existing proposals store the fee paid for their submission.
- Fails with `NOT_ADMIN` if the sender is not the administrator.

### **set_voting_period**

```ocaml
// Voting period in seconds
type voting_period = nat

Set_voting_period of voting_period
```

Parameter (in Michelson):
```
(nat %set_voting_period)
```

- Update how long the voting period should last.
- This affects all ongoing and new proposals.
- Voting period value is measured in seconds.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `OUT_OF_BOUND_VOTING_PERIOD` if the voting period value is out of the bound set by the configuration

### **set_quorum_threshold**

```ocaml
// Quorum threshold that a proposal needs to meet in order to be accepted,
// expressed as a fraction of the total_supply of frozen tokens.
// Invariant: numerator < denominator
type quorum_threshold =
  [@layout:comb]
  { numerator : nat
  ; denominator : nat
  }

Set_quorum_threshold of quorum_threshold
```

Parameter (in Michelson):
```
(pair %set_quorum_threshold (nat %numerator) (nat %denominator))
```

- Update the quorum threshold which proposals have to meet to not get rejected.
- The Quorum threshold is calculated as the proportion of total votes (upvotes
  and downvotes) over the frozen token's total supply.
- This affects all ongoing and new proposals.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `OUT_OF_BOUND_QUORUM_THRESHOLD` if the quorum threshold value is out of the bound set by the configuration or the numerator is not smaller than the denominator.

### **vote**

```ocaml
type proposal_key = bytes

type vote_type = bool

type permit =
  { key : key
  ; signature : signature
  }

type vote_param =
  [@layout:comb]
  { proposal_key : proposal_key
  ; vote_type : vote_type
  ; vote_amount : nat
  }

type vote_param_permited =
  { argument : vote_param
  ; permit : permit option
  }

Vote of vote_param_permited list
```

Parameter (in Michelson):
```
(list %vote
  (pair
    (pair %argument
      (bytes %proposal_key)
      (pair
        (bool %vote_type)
        (nat %vote_amount)
      )
    )
    (option %permit
      (pair
        (key %key)
        (signature %signature)
      )
    )
  )
)
```

- This implements permits mechanism similar to the one in [TZIP-017](https://gitlab.com/tzip/tzip/-/blob/23c5640db0e2242878b4f2dfacf159a5f6d2544e/proposals/tzip-17/tzip-17.md) but injected directly to the entrypoint.
- Vote author is identified by permit information, or if it is absent - `sender` is taken.
- Author MUST have frozen tokens equal to `voteAmount` or more (1 unstaked frozen token is needed for 1 vote) from past periods.
- Fails with `NOT_ENOUGH_FROZEN_TOKENS` if the frozen token balance of the author from past periods that is not staked
  is less than specified `voteAmount` .
- Fails with `PROPOSAL_NOT_EXIST` if the proposal key is not associated with any ongoing proposals.
- Fails with `VOTING_PERIOD_OVER` if the voting period of proposal associated with the proposal key has already ended.
- Fails with `MAX_VOTES_REACHED` if the amount of votes of the associated proposal is already at the max value set by the configuration.
- Fails with `MISSIGNED` if permit is incorrect with respect to the provided vote parameter and contract state.
- The entrypoint accepts a list of vote params. As a result, it is possible to `vote` on multiple proposals (or the same proposal multiple time) in one entrypoint call.

### **flush**

```ocaml
Flush of nat
```

Parameter (in Michelson):
```
(nat %flush)
```

- Finish voting process on an amount of proposals for which the voting period is over.
- The order of processing proposals are from 'the oldest' to 'the newest'. The proposals which
have the same timestamp due to being in the same block, are processed in the order of their proposal keys.
- Frozen tokens from voters and proposal submitter associated with those proposals are returned
  in form of tokens in governance token contract:
  - If proposal got rejected due to the quorum was not met or the quorum was met but upvotes are less then downvotes:
    - The return amount for the proposer is equal to or less than the slashed amount based on `rejectedProposalReturnValue`.
    - The paid fee is not returned to the proposer.
    - The return amount for each voters is equal to or less than the voter's frozen tokens.
  - If proposal got accepted:
    - The return amount for the proposer is equal to or less than the sum of the proposer frozen tokens and the fee paid for the proposal.
    - The return amount for each voters is equal to or less than the voter's frozen tokens.
- The lost of frozen tokens is due to the fact that the administrator has the right to `burn` or `transfer` frozen tokens of any proposers or voters.
- If proposal is accepted, decision lambda is called.

### **drop_proposal**

```ocaml
type proposal_key = bytes

Drop_proposal of proposal_key
```

Parameter (in Michelson):
```
(bytes %drop_proposal)
```

- Delete a finished and accepted proposal that is not flushed. Tokens frozen for this
proposal are returned to the proposer and voters in full. The decision lambda is skipped.
- This entrypoint should only be used when there is a proposal that is stuck due to having a
failing decision lambda.
- Fails with `NOT_ADMIN` if the sender is not the administrator.


[FA2]: https://gitlab.com/tzip/tzip/-/blob/3a6464b1e641008b77a83807a0c102e7602c6af4/proposals/tzip-12/tzip-12.md

## Views

### **Get_vote_permit_counter**

```ocaml
type vote_permit_counter_param =
  [@layout:comb]
  { param : unit
  ; postprocess : nat -> nat
  }

Get_vote_permit_counter of vote_permit_counter_param
```

Parameter (in Michelson):
```
(pair %get_vote_permit_counter
  (unit %param)
  (lambda %postprocess nat nat)
)
```

- A `void` entrypoint as defined in [TZIP-004](https://gitlab.com/tzip/tzip/-/blob/23c5640db0e2242878b4f2dfacf159a5f6d2544e/proposals/tzip-4/tzip-4.md#void-entrypoints).
- For `vote` entrypoint with permit, returns the current suitable counter for constructing permit signature.

### **get_total_supply**

```ocaml
type get_total_supply_param =
  [@layout:comb]
  { token_id : token_id
  ; postprocess : nat -> nat
  }

Get_total_supply of get_total_supply_param
```

Parameter (in Michelson):
```
(pair %get_total_supply
  (nat %token_id)
  (lambda %postprocess nat nat)
)
```

- A `void` entrypoint as defined in [TZIP-004](https://gitlab.com/tzip/tzip/-/blob/23c5640db0e2242878b4f2dfacf159a5f6d2544e/proposals/tzip-4/tzip-4.md#void-entrypoints).
- Return the total number of tokens for the given token id.
- Fail with `FA2_TOKEN_UNDEFINED` if the token with the given id has not been defined by the DAO.

## Custom entrypoints

BaseDAO allows DAOs to define their own additional entrypoints.

```ocaml
bytes * full_storage -> operation list * storage
```

where the bytes is the packed parameter of the custom entrypoint.

To call one of these "custom entrypoints" the contract `parameter` has:

### **CallCustom**

```ocaml
type custom_ep_param = (string * bytes)

CallCustom of custom_ep_param
```

Parameter (in Michelson):
```
(pair %callCustom
  string
  bytes
)
```

`CallCustom` receives:
- a `string`: the custom entrypoint name stored inside `custom_entrypoints` to execute
- a `bytes`: the `packed` representation of the `<ep_param>` to execute it with

### **freeze**

```ocaml
type freeze_param = nat

Freeze of freeze_param
```

Parameter (in Michelson):
```
(nat %freeze)
```

- Mints the required number of frozen tokens after making an FA2 transfer on
  the governance token contract from the address of the sender to the address
  of the BaseDAO contract. The transfer is made using the governance token id.
  The frozen tokens can only be used from the next period onward.

- Author MUST have tokens equal to `freeze_param` or more, in the governance contract, with token id
  'governance-token.token_id`.

### **unfreeze**

```ocaml
type unfreeze_param = nat

Unfreeze of unfreeze_param
```

Parameter (in Michelson):
```
(nat %unfreeze)
```

- Burns the specified amount of tokens from the tokens frozen in previous periods, after making an FA2 transfer
  on the governance contract from the address of the baseDAO contract to the address of the sender.
- Fails with `NOT_ENOUGH_FROZEN_TOKENS` if the author does not have enough tokens that can be burned.

# TZIP-016 metadata

This contract implements [TZIP-016](https://gitlab.com/tzip/tzip/-/blob/21fb73fe01df8a744c9b03303e3d73b0f2265eb2/proposals/tzip-16/tzip-16.md).

The DAO contract itself won't store the metadata, rather a dedicated contract will contain that.
Motivation:
* The baseDAO contract is very large and already over the Tezos hard limits.
  * Providing the metadata during origination would increase the already excessive origination cost;
  * Inserting the metadata after origination still requires mechanisms to manage the metadata, increasing the contract size even more.

## Deployment with metadata

The deployment of contract with metadata has an extra step: a dedicated contract for carrying metadata has to be originated first.
Then the baseDAO contract should include the reference to a metadata key in the contract in order to be compliant with TZIP-016.
