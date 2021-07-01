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

The contract described here implements the DAO functionality with proposals and voting.

An existing separate FA2 contract is required and used for governance.

# General Requirements

- The contract must store frozen tokens.

- The contract must store info about the external 'governance token'.
  This consist of the address of an FA2 contract and a `token_id` in that contract.
  The 'governance token' is used as part of the freeze/unfreeze process by
  performing FA2 transfers to/from it.

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

The latter keeps global information, for example about accepted proposals.
In this case, we associate a `string` "name" to the `pack`ed representation of
the data, that can then be `unpack`ed by the contract code.

For example, the `proposal_metadata` in a "treasury" style DAO would be the
packed version of the type `treasury_dao_proposal_metadata` :
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

type treasury_dao_proposal_metadata =
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
  { proposal_check : propose_params * contract_extra -> unit
  // ^ A lambda used to verify whether a proposal can be submitted.
  // It checks 2 things: the proposal itself and the amount of tokens frozen upon submission.
  // It allows the DAO to reject a proposal by arbitrary logic and captures bond requirements
  ; rejected_proposal_slash_value : proposal * contract_extra -> nat
  // ^ When a proposal is rejected, the value that voters get back can be slashed.
  // This lambda returns the amount to be slashed.
  ; decision_lambda : proposal * contract_extra -> operation list * contract_extra
  // ^ The decision lambda is executed based on a successful proposal.
  // It has access to the proposal, can modify `contractExtra` and perform arbitrary
  // operations.

  ; max_proposals : nat
  // ^ Determine the maximum number of ongoing proposals that are allowed in the contract.
  ; max_voters : nat
  // ^ Determine the maximum number of voters that are allowed to vote on a proposal.
  // Voters who votes on a proposal, both ways are counted twice.
  ; max_quorum_threshold : quorum_fraction
  // ^ Determine the maximum value of quorum threshold that is allowed.
  ; min_quorum_threshold : quorum_fraction
  // ^ Determine the minimum value of quorum threshold that is allowed.

  ; period : period
  // ^ Determines the stages length in number of blocks.

  ; fixed_proposal_fee_in_token : nat
  // ^ A base fee paid for submitting a new proposal.

  ; max_quorum_change : quorum_fraction
  // ^ A percentage value that limits the quorum_threshold change during
  // every update of the same.
  ; quorum_change : quorum_fraction
  // ^ A percentage value that is used in the computation of new quorum
  // threshold value.
  ; governance_total_supply : nat
  // ^ The total supply of governance tokens used in the computation of
  // of new quorum threshold value at each stage.

  ; proposal_flush_level : blocks
  // ^ The proposal age at (and above) which the proposal is considered flushable.
  // Has to be bigger than `period * 2`
  ; proposal_expired_level : blocks
  // ^ The proposal age at (and above) which the proposal is considered expired.
  // Has to be bigger than `proposal_flush_level`

  ; custom_entrypoints : custom_entrypoints
  // ^ Packed arbitrary lambdas associated to a name for custom execution.
  }
```

Note:
- see the [ligo source](../src/types.mligo) for more info about the types involved.
- `storage` is the storage type of the contract without the configuration.
- `full_storage` is instead the full storage of the contract, including its configuration,
which is to say: `type full_storage = storage * config`.

```ocaml
type proposal_key = bytes

type proposal =
  { upvotes : nat
  // ^ total amount of votes in favor
  ; downvotes : nat
  // ^ total amount of votes against
  ; start_level : blocks
  // ^ block level of submission, used to order proposals
  ; voting_stage_num : nat
  // ^ stage number in which it is possible to vote on this proposal
  ; metadata : proposal_metadata
  // ^ instantiation-specific additional data
  ; proposer : address
  // ^ address of the proposer
  ; proposer_frozen_token : nat
  // ^ amount of frozen tokens used by the proposer, exluding the fixed fee
  ; voters : voter list
  // ^ voter data
  ; quorum_threshold: quorum_threshold
  // ^ quorum threshold at the cycle in which proposal was raised
  }
```

## Storage configuration

Part of the configuration is specified inside the `storage` and is fixed during
the contract lifetime after being set at origination.
These values are:
1. `admin : address` is the address that can perform administrative actions.
2. `guardian : address` is the address of a contract that has permission to call
   `drop_proposal` on any proposals.
   Note: the `guardian` contract cannot initiate the transaction that results
   in a call to `drop_proposal`.
3. `governance_token` is the FA2 contract address/token_id pair that will be
   used as the governance token.
4. `period : blocks` specifies how long the stages lasts in blocks.
5. `proposal_flush_level : blocks`
    - Specifies, in blocks, how long it takes before a proposal can be flushed,
      from when it was proposed.
    - IMPORTANT: Must be bigger than `period * 2`.
6. `proposal_expired_level : blocks`
    - Specifies, in blocks, how long it takes for a proposal to be considered
      expired, from when it was proposed.
    - IMPORTANT: Must be bigger than `proposal_flush_level`.
7. `quorum_threshold : quorum_threshold` specifies what fraction of the frozen
   tokens total supply are required in total to vote for a successful proposal.
8. `fixed_proposal_fee_in_token : nat` specifies the fee to be paid for submitting
   a proposal (in frozen tokens), if any.

# Contract logic

This chapter provides a high-level overview of the contract's logic.

- The contract maintains a map of addresses and their frozen token balance.
- The contract maintains the address of an a FA2 contract and a `token_id`,
  to use in the governance process.
- The contract manages three special roles, the `admin`, `guardian`, and `delegate`.
- The contract stores a list of proposals that can be in one of the states:
  "proposed", "ongoing", "pending flush", "accepted", "rejected", or "expired".
- The contract forbids transferring XTZ to it on certain entrypoints.
- The contract tracks 'stages' by counting the blocks.

## Roles

The token supports two "global" user role: `admin` and `guardian`.
These roles apply to the whole contract (hence "global"):

* **admin**
  - Can re-assign this role to a new address or to the DAO itself.
  - Can perform administrative operations.
  - There always must be exactly one `admin`.
* **guardian**
  - Can drop any proposal at any time.
  - Cannot be an implicit address, in other words it must be a contract.
  - There always must be exactly one `guardian`.
  - Can be updated via a proposal.


Additionally, the contract also contains the **delegate** role:
  - This role is "local" to a particular address.
  - Each address can have any number of delegates and be a delegate of any number of addresses.
  - This role can call `propose` and `vote` on behalf of the owner.

## Period, Stages and Cycles

The contract constantly cycles between two `stage`s, a proposing `stage` and a voting `stage`.
Both have the same same length, called `period`, and alternate between each other,
starting from "voting" for `stage` number `0`.
A proposing and voting couple of `stage`s is called a `cycle`.

The `period` is specified for the whole smart contract and never changes.

The length of a period is measured by counting blocks as discrete entities. So
if the configuration value of period is `3`, then the very first period only
exist for blocks `0`, `1` and `2`.

Similarly a proposal raised in block `100`, with an expiry of `3` blocks will
remain unexpired for blocks `100`, `101`, `102`, and will be considered expired
on the `103` th block, because at that block, the proposal will be considered to
have an age of `3`.

Tokens can be frozen in any `stage`, but they can only be used for voting, proposing
and unfreezing starting from the one following and onwards.

For this reason the contract starts from a voting `stage`, because even tho there
are no proposals to vote on yet, this allows token to be frozen in it and be
usable in the first proposing `stage`, number `1`.

To `freeze`, the address should have the corresponding amount of tokens of the
proper `token_id` in the governance FA2 contract.
During the `freeze` operation the DAO contract performs an FA2 `transfer` call
on the governance contract to transfer tokens to its own address from
the address who is freezing the tokens and then mints frozen tokens for the address.

Unfreezing does the opposite, that is, the contract makes the FA2 `transfer` call
on the governance contract to transfer tokens from its own address to the address
that is doing the unfreezing and burns the corresponding amount of frozen tokens.
Only frozen tokens that are not currently staked in a vote or proposal can be unfrozen.

The quorum threshold is updated at every `cycle` change, based on the previous
cycle participation using the formula:

```
previous participation = number_of_staked_tokens_last_cycle / config.governance_total_supply.

possible_new_quorum =
  old_quorum * (1 - config.quorum_change) + participation * quorum_change

min_new_quorum =
  old_quorum / (1 + config.max_quorum_change)

max_new_quorum =
  old_quorum * (1 + config.max_quorum_change)

new_quorum =
  max min_new_quorum
  (min max_new_quorum
       possible_new_quorum)
```

This will use the configuration values provided at origination, and the new
quorum will be still bound by the max/min quorum threshold values provided there.

### Proposals

Everyone can make a new proposal, however, one has to `freeze` some tokens for that.
The proposer specifies how many frozen tokens they want to stake and this value
is checked by the contract according to its configuration.

Proposing can only be performed in a proposing `stage`, meaning one that's
odd-numbered and the proposer must have frozen his tokens in one of the preceding
`stage`s.

Proposals are identified by a key which is a `bytes` value computed via the Blake2B
hashing function of a pair of propose entrypoint params and the proposer address.

### Voting

Once a proposal is submitted, everyone can vote on it as long as they have enough
frozen tokens to stake. One frozen token is required for each vote.

A vote can only be cast in a voting `stage`, meaning one that's even-numbered.
Moreover the proposal to vote on must have been submitted in the proposing `stage`
immediately preceding and the voter must have frozen his tokens in one of the
preceding `stage`s.

Each vote stakes one frozen token. Staked tokens cannot be unfreezed till they
are unstaked when the associated proposal is flushed. The number of staked tokens
only depend on the number of votes, and does not depend on whether the vote is in
favor or against a proposal.

It's possible to vote positively or negatively.
After the voting ends, the contract is "flushed" by calling a dedicated entrypoint.

# Errors

In error scenarios the contract fails with a string or a pair where the first
item is a string.

Here is a summary of all the strings used as error messages.
(The list of errors may be inaccurate and incomplete, it will be updated during the implementation.)

| Error                                | Description                                                                                                              |
|--------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| `NOT_ADMIN`                          | The sender is not the administrator                                                                                      |
| `NOT_PENDING_ADMIN`                  | Authorized sender is not the current pending administrator                                                               |
| `NOT_TOKEN_OWNER`                    | Trying to configure operators for a different wallet which sender does not own                                           |
| `FAIL_PROPOSAL_CHECK`                | Throws paired with an additional `string` message when trying to submit a proposal that does not pass `proposal_check`.  |
| `FROZEN_TOKEN_NOT_TRANSFERABLE`      | Transfer entrypoint is called for frozen token                                                                           |
| `PROPOSAL_NOT_EXIST`                 | Throws when trying to vote on a proposal that does not exist or is no longer ongoing.                                    |
| `QUORUM_NOT_MET`                     | A proposal is flushed, but there are not enough votes                                                                    |
| `VOTING_STAGE_OVER`                  | Throws when trying to vote on a proposal that is already ended                                                           |
| `MAX_PROPOSALS_REACHED`              | Throws when trying to propose a proposal when proposals max amount is already reached                                    |
| `MAX_VOTERS_REACHED`                 | Throws when trying to vote on a proposal when the max voter count of that proposal is already reached                    |
| `FORBIDDEN_XTZ`                      | Throws when some XTZ was received as part of the contract call                                                           |
| `PROPOSER_NOT_EXIST_IN_LEDGER`       | Expect a proposer address to exist in Ledger but it is not found                                                         |
| `PROPOSAL_NOT_UNIQUE`                | Trying to propose a proposal that is already existed in the Storage.                                                     |
| `MISSIGNED`                          | Parameter signature does not match the expected one - for permits.                                                       |
| `ENTRYPOINT_NOT_FOUND`               | Throw when `CallCustom` is called with a non-existing entrypoint                                                         |
| `UNPACKING_FAILED`                   | Throw when unpacking of a stored entrypoint, its parameter or a required `extra` value fails.                            |
| `UNPACKING_PROPOSAL_METADATA_FAILED` | Throw when unpacking of a proposal metadata fails.                                                                       |
| `MISSING_VALUE`                      | Throw when trying to unpack a field that does not exist.                                                                 |
| `NOT_PROPOSING_STAGE`                | Throw when `propose` call is made on a non-proposing period.                                                             |
| `NOT_ENOUGH_FROZEN_TOKENS`           | Throw when there is not enough frozen tokens for the operation.                                                          |
| `BAD_TOKEN_CONTRACT`                 | Throw when the token contract is not of expected type.                                                                   |
| `BAD_VIEW_CONTRACT`                  | Throw when the contract for a view entrypoint is not of the expected type.                                               |
| `DROP_PROPOSAL_CONDITION_NOT_MET`    | Throw when calling `drop_proposal` when the sender is not proposer or guardian and proposal is not expired.              |
| `EXPIRED_PROPOSAL`                   | Throw when trying to `flush` expired proposals.                                                                          |
| `EMPTY_FLUSH`                        | Thrown when trying to `flush` with no available proposals.                                                               |
| `BAD_STATE`                          | Throw when storage is in an unexpected state, indicating a contract error.                                               |
| `NOT_DELEGATE`                       | Throw when calling `propose` and `vote` with the sender that is not `from` or delegate of `from`.                        |

# Entrypoints

Full list:
* [`transfer_contract_tokens`](#transfer_contract_tokens)
* [`transfer_ownership`](#transfer_ownership)
* [`accept_ownership`](#accept_ownership)
* [`update_delegates`](#update_delegates)
* [`propose`](#propose)
* [`vote`](#vote)
* [`flush`](#flush)
* [`drop_proposal`](#drop_proposal)
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

## Token Entrypoints

Functions related to token transfers.

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
  { from : address
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
    (pair (address %from)
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
  `accept_ownership` is called, unless the proposed administrator is the DAO itself.
  In such case, the role is given to the DAO right away.

- Can be called multiple times, each call replaces pending administrator with
  the new one. Note, that if the proposed administrator is the same as the current
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

- Fails with `NOT_PENDING_ADMIN` if the sender is not the current pending
  administrator, this also includes the case when pending administrator was not set.

- When pending administrator is not set, it is considered equal to the current owner,
  thus administrator can accept ownership of its own contract without a prior `transfer_ownership` call.


### **update_delegates**

```ocaml
type update_delegate =
  [@layout:comb]
  { enable : bool
  ; delegate : address
  }

type update_delegate_params = update_delegate list

Update_delegate of update_delegate_params
```

Parameter (in Michelson)
```
(list %update_delegate
  (pair (bool %enable) (address %delegate)
  )
)
```

- Add/Update or remove delegates of owners. The owner address is taken from `SENDER`.

## Proposal entrypoints

### **propose**

```ocaml
type proposal_metadata = bytes

type propose_params =
  { from : address
  ; frozen_token : nat
  ; proposal_metadata : proposal_metadata
  }

Propose of propose_params
```

Parameter (in Michelson):
```
(pair %propose
  (address %from)
  (pair (nat %frozen_token) (bytes %proposal_metadata)))
```

- The `proposer` address is taken from `from`.
- Fails with `NOT_DELEGATE` if the `SENDER` address is not equal to `proposer` address or the `delegate` address of the proposer.
- The proposal is saved under `BLAKE2b` hash of proposal value and proposer.
- The `Natural` value: `proposalTokenAmount` determines how many proposer's frozen
  tokens will be staked in addition to the [fee](#configuration)
- Proposer MUST have enough frozen tokens (i. e. `≥ proposalTokenAmount + fee`) that
  are not already staked for a proposal or a vote.
- Fails with `NOT_ENOUGH_FROZEN_TOKENS` if the unstaked frozen token balance of the proposer
  is less than `proposalTokenAmount + fee`.
- Fails with `NOT_PROPOSING_STAGE` if the current stage is not a proposing one.
- Fails with `FAIL_PROPOSAL_CHECK` if the proposal is rejected by `proposal_check`
  from the configuration.
- Fails with `MAX_PROPOSALS_REACHED` if the current amount of ongoing proposals is at max value set by the config.
- Fails with `PROPOSAL_NOT_UNIQUE` if exactly the same proposal from the same author has been proposed.


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
  ; from : address
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
  (pair (pair %argument
          (pair (address %from) (bytes %proposal_key))
          (pair (nat %vote_amount) (bool %vote_type)))
        (option %permit (pair (key %key) (signature %signature)))))
```

- This implements permits mechanism similar to the one in [TZIP-017](https://gitlab.com/tzip/tzip/-/blob/23c5640db0e2242878b4f2dfacf159a5f6d2544e/proposals/tzip-17/tzip-17.md)
  but injected directly to the entrypoint.
- The `author` is identified by permit information, or if it is absent - `SENDER` is taken.
- The `voter` address is taken from `from`.
- Fails with `NOT_DELEGATE` if the `author` address is not equal to `voter` address or the `delegate` address of the `voter`.
- The voter MUST have frozen tokens equal to `vote_amount` or more
  (1 unstaked frozen token is needed for 1 vote) from past `stage`s.
- Fails with `NOT_ENOUGH_FROZEN_TOKENS` if the frozen token balance of the voter
  from past stages that is not staked is less than specified `vote_amount` .
- Fails with `PROPOSAL_NOT_EXIST` if the proposal key is not associated with any ongoing proposals.
- Fails with `VOTING_STAGE_OVER` if the voting `stage` for the proposal has already ended.
- Fails with `MAX_VOTERS_REACHED` if the voter count of the associated proposal
  is already at the max value set by the configuration.
- Fails with `MISSIGNED` if permit is incorrect with respect to the provided vote parameter and contract state.
- The entrypoint accepts a list of vote params. As a result, it is possible to
  `vote` on multiple proposals (or the same proposal multiple time) in one entrypoint call.

### **flush**

```ocaml
Flush of nat
```

Parameter (in Michelson):
```
(nat %flush)
```

- Finish voting process on an amount of proposals for which their `proposal_flush_level`
  was reached, but their `proposal_expire_level` wasn't yet.
- The order of processing proposals are from 'the oldest' to 'the newest'.
  The proposals which have the same level due to being in the same block,
  are processed in the order of their proposal keys.
- Frozen tokens from voters and proposal submitter associated with those proposals
  are returned in the form of tokens in governance token contract:
  - If proposal got rejected due to the quorum was not met or the quorum was met but upvotes are less then downvotes:
    - The return amount for the proposer is equal to or less than the slashed amount based on `rejected_proposal_slash_value`.
    - The paid fee is not returned to the proposer.
    - The return amount for each voters is equal to or less than the voter's frozen tokens.
  - If proposal got accepted:
    - The return amount for the proposer is equal to or less than the sum of the proposer frozen tokens and the fee paid for the proposal.
    - The return amount for each voters is equal to or less than the voter's frozen tokens.
- If proposal is accepted, the decision lambda is called.
- The `quorum_threshold` at the cycle in which the proposal was raised will be
  stored in the proposal, and this threshold will be used to check if the votes
  meet the quorum threshold.
  So any dynamic update to the quorum threshold shall not affect the proposal.
- If no proposals can be flushed when called, fails with `EMPTY_FLUSH`.

### **drop_proposal**

```ocaml
type proposal_key = bytes

Drop_proposal of proposal_key
```

Parameter (in Michelson):
```
(bytes %drop_proposal)
```

- Delete a proposal when either:
  - The `proposal_expired_level` has been reached.
  - The proposer is the `SENDER`, regardless of the `proposal_expired_level`.
  - The `guardian` is the `SENDER`, regardless of the `proposal_expired_level`.
- Fails with `DROP_PROPOSAL_CONDITION_NOT_MET` when none of the conditions above are met.
- Tokens that are frozen for this proposal are returned to the proposer and voters
  as if the proposal was rejected, regardless of the actual votes.

[FA2]: https://gitlab.com/tzip/tzip/-/blob/3a6464b1e641008b77a83807a0c102e7602c6af4/proposals/tzip-12/tzip-12.md

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
  The frozen tokens can only be used from the next `stage` onward.

- Author MUST have tokens equal to `freeze_param` or more, in the governance
  contract, with token id `governance-token.token_id`.


### **unfreeze**

```ocaml
type unfreeze_param = nat

Unfreeze of unfreeze_param
```

Parameter (in Michelson):
```
(nat %unfreeze)
```

- Burns the specified amount of tokens from the tokens frozen in previous `stage`s,
  after making an FA2 transfer on the governance contract from the address of the
  baseDAO contract to the address of the sender.
- Fails with `NOT_ENOUGH_FROZEN_TOKENS` if the author does not have enough tokens
  that can be burned.

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

# TZIP-016 metadata

This contract implements [TZIP-016](https://gitlab.com/tzip/tzip/-/blob/21fb73fe01df8a744c9b03303e3d73b0f2265eb2/proposals/tzip-16/tzip-16.md).

The DAO contract itself won't store the metadata, rather a file on IPFS (suggested),
a dedicated contract or another external storage will contain that.

## Deployment with metadata

The deployment of contract with metadata has an extra step, either:
  - Uploading the contract metadata to IPFS.
  - A dedicated contract for carrying metadata has to be originated first.

Then the baseDAO contract should include the reference to a metadata key in the
contract in order to be compliant with TZIP-016.
