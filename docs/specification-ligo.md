<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Table of contents

* [Overview](#overview)
* [General requirements](#general-requirements)
* [Configuration](#configuration)
* [Contract logic](#contract-logic)
* [Errors](#errors)
* [Entrypoints](#entrypoints)
* [TZIP-16 metadata](#tzip-16-metadata)

# Overview

The contract described here consists of two parts:
- Token functionality ([FA2-based][FA2]).
- DAO functionality with proposals and voting.

These two parts are coupled into one smart contract because interaction between smart contracts in Tezos is expensive and hard to get right.

# General Requirements

- The contract must be FA2 compatible.

- The contract must store tokens of two types: frozen (`token_id` is 1) and unfrozen (`token_id` is 0).

- The storage of the contract must have annotations for all fields
  and must be documented to make its interpretation easy for users.

# Configuration

BaseDAO is a concrete smart contract, but also a framework to implement various DAOs.
It can be configured at origination for any specific needs.

In order to do so the contract has two type synonyms for `(string, bytes) map`
(or in Michelson: `map string bytes`), that can contain arbitrary data:
`proposal_metadata` and `contract_extra`.

The former contains fields that are required to submit a proposal.
The latter keeps global information like information about accepted proposals.

In both cases we associate a `string` "name" to the `pack`ed representation of
the data, that can then be `unpack`ed by the contract code.

For example a "treasury" style DAO would have a `proposal_metadata` containing:
```
"mutezAmount": <packed mutez>
"recipientId": <packed string>
"agoraPostId": <packed nat>
```
and an empty `contract_extra`.

Lastly, there is one more `(string, bytes) map` type synonym: `custom_entrypoints`,
used to execute arbitrary logic on the contract, see [its section](#custom-entrypoints)
for more information on its content and usage.

DAO configuration value parameters are captured by the `config` type:

```cameLIGO
type config =
  { unfrozen_token_metadata : token_metadata
  // ^ FA2 metadata for unfrozen token.
  ; frozen_token_metadata : token_metadata
  // ^ FA2 metadata for frozen token.
  ; proposal_check : propose_params * storage -> bool
  // ^ A lambda used to verify whether a proposal can be submitted.
  // It checks 2 things: the proposal itself and the amount of tokens frozen upon submission.
  // It allows the DAO to reject a proposal by arbitrary logic and captures bond requirements
  ; rejected_proposal_return_value : proposal * storage -> nat
  // ^ When a proposal is rejected, the value that voters get back can be slashed.
  ; decision_lambda : proposal * storage -> operation list * storage
  // ^ The decision lambda is executed based on a successful proposal.
  // It has access to the proposal, can modify `contractExtra` and perform arbitrary
  // operations.

  ; max_proposals : nat
  // ^ Determine the maximum number of ongoing proposals that are allowed in the contract.
  ; max_votes : nat
  // ^ Determine the maximum number of votes associated with a proposal including positive votes
  ; max_quorum_threshold : nat
  // ^ Determine the maximum value of quorum threshold that is allowed to be set.
  ; min_quorum_threshold : nat
  // ^ Determine the minimum value of quorum threshold that is allowed to be set.
  ; max_voting_period : nat
  // ^ Determine the maximum value of voting period that is allowed to be set.
  ; min_voting_period : nat
  // ^ Determine the minimum value of voting period that is allowed to be set.

  ; custom_entrypoints : custom_entrypoints
  // ^ Packed arbitrary lambdas associated to a name for custom execution.
  }
```

Note:
- the `token_metadata` type matches the one defined in FA2.
- the `proposal` type is defined below.
- `storage` is the storage type of the contract without the configuration.
- `full_storage` is instead the full storage of the contract, including its configuration,
which is to say: `type full_storage = storage * config`.


```cameLIGO
type proposal_key = bytes

type proposal =
  { upvotes : nat
  ; downvotes : nat
  ; start_date : timestamp
  ; metadata : proposal_metadata
  ; proposer : address
  ; proposer_frozen_token : nat
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
2. `voting_period : nat` specifies how long the voting period lasts.
3. `quorum_threshold : nat` specifies how many total votes are required for a successful proposal.

# Contract logic

This chapter provides a high-level overview of the contract's logic.

- The contract maintains a ledger of address and its balance (frozen and unfrozen tokens)
- The contract manages a special role called "Administrator".
- The contract stores a list of proposals that can be in one of the states: "ongoing", "rejected", or "accepted".
- Migration forms a two-step process:
  + After the migration starts, the contract `migration_status` state is set to `MigratingTo targetAddress` state.
  + After successful migration confirmation, `migration_status` is set to `MigratedTo newContractAddress` state. All the subsequent operations, except `transfer_contract_tokens`, will fail with the `MIGRATED` tag.
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

## Proposals

Everyone can make a new proposal, however, you have to freeze some tokens for that.
The proposer specifies how many tokens they want to freeze and this value is checked by
the contract according to its compile-time configuration.
If this value is accepted and the proposer has enough unfrozen tokens, these unfrozen tokens
are frozen and voting starts.

Proposals are identified by a key which is a `bytes` value computed via Blake2B hashing function of a
pair of propose entrypoint params and the proposer address.

## Voting

Once a proposal is submitted, everyone can vote on it as long as they have enough unfrozen tokens.
One unfrozen token is required for one vote.
The tokens are frozen for the duration of voting.
Voting period is specified for the whole smart contract and can be updated by the administrator; on update, the existing proposals are also affected.
It's possible to vote positively or negatively.
After the voting ends, the contract is "flushed" by calling a dedicated entrypoint.

## Ledger

Every address that is stored in ledger is associated with its unfrozen token balance and
frozen token balance.
When unfrozen tokens are transferred, the balance of the `from_` addresses is decreased
and the balance of the `to_` addresses is increased according to the transferred values.

# Errors

In error scenarios the baseDAO contract fails with a string.
Here is a summary of all the strings used as error messages.
We start with standard FA2 errors which are part of the FA2 specification.

| Error                      | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `FA2_TOKEN_UNDEFINED`      | One of the specified `token_id`s is not defined                             |
| `FA2_INSUFFICIENT_BALANCE` | Cannot debit from a wallet because of excessive amount of tokens            |
| `FA2_NOT_OPERATOR`         | A transfer is initiated neither by the token owner nor a permitted operator |

The next group consists of the errors that are not part of the FA2 specification.
The list of erros may be inaccurate and incomplete, it will be updated during the implementation.

| Error                           | Description                                                                                                 |
|---------------------------------|-------------------------------------------------------------------------------------------------------------|
| `NOT_ADMIN`                     | The sender is not the administrator                                                                         |
| `NOT_PENDING_ADMIN`             | Authorized sender is not the current pending administrator                                                  |
| `NOT_TOKEN_OWNER`               | Trying to configure operators for a different wallet which sender does not own                              |
| `FAIL_TRANSFER_CONTRACT_TOKENS` | Trying to cross-transfer DAO tokens to another contract that does not exist or is not a valid FA2 contract. |
| `FAIL_PROPOSAL_CHECK`           | Throws when trying to propose a proposal that does not pass `proposalCheck`                                 |
| `FROZEN_TOKEN_NOT_TRANSFERABLE` | Transfer entrypoint is called for frozen token by a non-admin sender                                        |
| `PROPOSAL_INSUFFICIENT_BALANCE` | Throws when trying to propose a proposal without having enough unfrozen token                               |
| `VOTING_INSUFFICIENT_BALANCE`   | Throws when trying to vote on a proposal without having enough unfrozen token                               |
| `PROPOSAL_NOT_EXIST`            | Throws when trying to vote on a proposal that does not exist                                                |
| `QUORUM_NOT_MET`                | A proposal is flushed, but there are not enough votes                                                       |
| `VOTING_PERIOD_OVER`            | Throws when trying to vote on a proposal that is already ended                                              |
| `OUT_OF_BOUND_VOTING_PERIOD`    | Throws when trying to set voting period that is out of bound from what is specified in the `config`         |
| `OUT_OF_BOUND_QUORUM_THRESHOLD` | Throws when trying to set quorum threshold that is out of bound from what is specified in the `config`      |
| `MAX_PROPOSALS_REACHED`         | Throws when trying to propose a proposal when proposals max amount is already reached                       |
| `MAX_VOTES_REACHED`             | Throws when trying to vote on a proposal when the votes max amount of that proposal is already reached      |
| `MIGRATED`                      | Throw when conract has been migrated                                                                        |
| `NOT_MIGRATING`                 | Throw when `confirm_migration` is called and contract is not in migration                                   |
| `NOT_MIGRATION_TARGET`          | Throw when `confirm_migration` is called by address other than the new address                              |
| `FORBIDDEN_XTZ`                 | Throws when some XTZ was received as part of the contract call                                              |
| `PROPOSER_NOT_EXIST_IN_LEDGER`  | Expect a proposer address to exist in Ledger but it is not found                                            |
| `PROPOSAL_NOT_UNIQUE`           | Trying to propose a proposal that is already existed in the Storage.                                        |
| `MISSIGNED`                     | Parameter signature does not match the expected one - for permits.                                          |
| `ENTRYPOINT_NOT_FOUND`          | Throw when `CallCustom` is called with a non-existing entrypoint                                            |
| `UNPACKING_FAILED`              | Throw when unpacking of the stored lambda in a 'CallCustom' call fails                                      |

# Entrypoints

Full list:
* [`transfer`](#transfer)
* [`balance_of`](#balance_of)
* [`token_metadata_registry`](#token_metadata_registry)
* [`update_operators`](#update_operators)
* [`mint`](#mint)
* [`burn`](#burn)
* [`transfer_contract_tokens`](#transfer_contract_tokens)
* [`transfer_ownership`](#transfer_ownership)
* [`accept_ownership`](#accept_ownership)
* [`propose`](#propose)
* [`set_voting_period`](#set_voting_period)
* [`set_quorum_threshold`](#set_quorum_threshold)
* [`vote`](#vote)
* [`flush`](#flush)
* [`drop_proposal`](#drop_proposal)
* [`migrate`](#migrate)
* [`confirm_migration`](#confirm_migration)

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

```cameLIGO
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

- Although the contract supports two types of tokens: frozen token (`token_id = 1`) and unfrozen token (`token_id = 0`). All `token_id` values passed to this entrypoint by non-admin MUST be 0.

<!--
- If the destination address is the `freeze_my_tokens` address:
  - This entrypoint MUST update the unfrozen token balance(`token_id = 0`) according to FA2 requirement.
  - It MUST also increase the frozen token balance (`token_id = 1`) of the source address by the amount specified in the parameter.
-->

- The administrator can transfer tokens from any address to any address. He also can transfer frozen tokens.

### **balance_of**

```cameLIGO
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

- Since the contract supports two types of tokens: frozen token (`token_id = 1`) and unfrozen token (`token_id = 0`), all `token_id` values passed MUST be either 0 or 1.

### **token_metadata_registry**

```cameLIGO
type token_metadata_registry_param = address contract

Token_metadata_registry of token_metadata_registry_param
```

Parameter (in Michelson)
```
(contract %token_metadata_registry address)
```

- Return contract address that holds token metadata.
- Token metadata MUST include the DAO metadata.

### **update_operators**

```cameLIGO
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

- Although the contract supports two types of tokens: frozen token (`token_id = 1`) and unfrozen token (`token_id = 0`), all `token_id` values passed to this entrypoint MUST be 0, because transfers are supported only for unfrozen tokens.

- Each `owner` must be equal to `SENDER`, otherwise `NOT_TOKEN_OWNER` error occurs.

## Custom (non-FA2) token functions

Functions related to token transfers, but not present in FA2. They do not have `token_id` argument because only unfrozen token is supported (`token_id = 0`) and not necessary (since they are not part of FA2).

### **mint**

```cameLIGO
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

### **burn**

```cameLIGO
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

```cameLIGO
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

```cameLIGO
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

```cameLIGO
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

```cameLIGO
type proposal_metadata = (string, bytes) map

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
  (map %proposal_metadata string bytes)
)
```

- The proposal is saved under `BLAKE2b` hash of proposal value and sender.
- The `Natural` value: `proposalTokenAmount` determines how many sender's tokens will be frozen.
- Sender MUST have enough unfrozen tokens (i. e. `≥ proposalTokenAmount`).
- Fails with `PROPOSAL_INSUFFICIENT_BALANCE` if the unfrozen token balance of the SENDER
  is less than `proposalTokenAmount`.
- Fails with `FAIL_PROPOSAL_CHECK` if the proposal is rejected by `proposalCheck` from the configuration.
- Fails with `MAX_PROPOSALS_REACHED` if the current amount of ongoing proposals is at max value set by the config.
- Fails with `PROPOSAL_NOT_UNIQUE` if exactly the same proposal from the same author has been proposed.
- The sender's balance in frozen tokens is increased by `proposalTokenAmount` and in unfrozen tokens is decreased by `proposalTokenAmount`.

### **set_voting_period**

```cameLIGO
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

```cameLIGO
// Quorum threshold that a proposal need to meet
// quorum_threshold = upvote + downvote
type quorum_threshold = nat

Set_quorum_threshold of quorum_threshold
```

Parameter (in Michelson):
```
(nat %set_quorum_threshold)
```

- Update the quorum threshold value which proposals have to met to not get rejected.
- Quorum threshold value is calculated by adding the number of upvotes with downvotes.
- This affects all ongoing and new proposals.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `OUT_OF_BOUND_QUORUM_THRESHOLD` if the voting period value is out of the bound set by the configuration

### **vote**

```cameLIGO
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

- This implements permits mechanism similar to the one in TZIP-17 but injected directly to the entrypoint.
- Vote author is identified by permit information, or if it is absent - `sender` is taken.
- Author MUST have unfrozen tokens equal to `voteAmount` or more (1 unfrozen token is needed for 1 vote).
- Fails with `VOTING_INSUFFICIENT_BALANCE` if the unfrozen token balance of the author
  is less than specified `voteAmount` .
- Fails with `PROPOSAL_NOT_EXIST` if the proposal key is not associated with any ongoing proposals.
- Fails with `VOTING_PERIOD_OVER` if the proposal associated with the proposal key is already ended.
- Fails with `MAX_VOTES_REACHED` if the amount of votes of the associated proposal is already at the max value set by the configuration.
- Fails with `MISSIGNED` if permit is incorrect with respect to the provided vote parameter and contract state.
- The author's balance in frozen tokens is increased by `voteAmount` and in unfrozen tokens is decreased by `voteAmount`.
- The entrypoint accepts a list of vote params. As a result, it is possible to `vote` on multiple proposals (or the same proposal multiple time) in one entrypoint call.

### **flush**

```cameLIGO
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
  in form of unfrozen tokens:
  - If proposal got rejected due to the quorum was not met or the quorum was met but upvotes are less then downvotes:
    - The return amount for the proposer is equal to or less than the slashed amount based on `rejectedProposalReturnValue`.
    - The return amount for each voters is equal to or less than the voter's frozen tokens.
  - If proposal got accepted:
    - The return amount for the proposer is equal to or less than proposer frozen tokens.
    - The return amount for each voters is equal to or less than the voter's frozen tokens.
- The lost of frozen tokens is due to the fact that the administrator has the right to `burn` or `transfer` frozen tokens of any proposers or voters.
- If proposal is accepted, decision lambda is called.

### **drop_proposal**

```cameLIGO
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

## Migrating functions

### **migrate**

```cameLIGO
type migrate_param = address

Migrate of migrate_param
```

Parameter (in Michelson):
```
(address %migrate)
```

- After a successful `migrate` call, the contract will be in a state where it
  will accept a `confrimMigration` call from the new contract address. Until this
  call is received, the contract is not considered to be migrated, and will
  continue normal operations.
- The address denotes the new DAO address.
- Overwrites the new address from any previous `migrate` calls.
- Fails with `NOT_ADMIN` if the sender is not the administrator.

### **confirm_migration**

```cameLIGO
Confirm_migration of unit
```

Parameter (in Michelson):
```
(unit %confirm_migration)
```

- After a successful `confirm_migration` call, the contract is permanently set
  to the migrated state, no operations are possible. All contract calls, except
  `transfer_contract_tokens`, fail with `"MIGRATED"`.
  The `transfer_contract_tokens` entrypoint will continue to work
  even after migration is completed.
- Fails with `NOT_MIGRATING` if `migrate` operation has not been called yet.
- Fails with `NOT_MIGRATION_TARGET` if the sender of this call is not the address of the new version.

[FA2]: https://gitlab.com/tzip/tzip/-/blob/3a6464b1e641008b77a83807a0c102e7602c6af4/proposals/tzip-12/tzip-12.md

## Views

### **GetVotePermitCounter**

```cameLIGO
type vote_permit_counter_param =
  [@layout:comb]
  { param : unit
  ; callback : nat contract
  }

GetVotePermitCounter of vote_permit_counter_param
```

Parameter (in Michelson):
```
(pair %getVotePermitCounter
  (unit %param)
  (contract %callback nat)
)
```

- For `vote` entrypoint with permit, returns the current suitable counter for constructing permit signature.

## Custom entrypoints

BaseDAO allows DAOs to define their own additional entrypoints.

This is done with the use of the `config` option `custom_entrypoints`, which is
a `(string, bytes) map` that associates a `string` "entrypoint name" with the
`bytes` of a `pack`ed lambda with the signature:

```cameLIGO
bytes * full_storage -> operation list * storage
```

where the bytes is the packed parameter of the custom entrypoint.

To call one of these "custom entrypoints" the contract `parameter` has:

### **CallCustom**

```cameLIGO
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

# TZIP-16 metadata

Note: we do not provide a proper support for contract metadata until [the proposal](https://gitlab.com/tzip/tzip/-/merge_requests/94) is finalized and merged.

This contract implements TZIP-16.

The DAO contract itself won't store the metadata, rather a dedicated contract will contain that.
Motivation:
* The baseDAO contract is very large and approaches the Tezos hard limits.
  * Providing the metadata during origination would significantly reduce the amount of functionality we can put into contract;
  * Inserting the metadata after origination still requires mechanisms to manage the metadata, increasing the contract size.

## Deployment with metadata

The deployment of contract with metadata has an extra step: a dedicated contract for carrying metadata has to be originated first.
Then the baseDAO contract should include the reference to a metadata key in the contract in order to be compliant with TZIP-16.
