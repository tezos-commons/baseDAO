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

These two parts are coupled into one smart contract because interaction between smart contracts in Tezos is expensive and hard to get right.

# General Requirements

- The contract must be FA2 compatible.

- The contract must store tokens of two types: frozen (`token_id` is 1) and unfrozen (`token_id` is 0).

- The storage of the contract must have annotations for all fields
  and must be documented to make its interpretation easy for users.

# Configuration

BaseDAO is not a concrete smart contract, but rather a framework to implement various DAOs.
It can be configured in compile-time for any specific needs.
"Compile-time" configuration refers to the compilation of the smart contract to Michelson.
That is, in order to get Michelson version of the contract one has to configure it first.

The smart contract is implemented in Haskell eDSL Lorentz which allows us to have 2 types of configuration:
1. Type parameters that should be instantiated to specific types by DAO creator.
2. Michelson constants, including lambdas, that can be passed as Haskell values.

BaseDAO has 2 type parameters `proposalMetadata` and `contractExtra` that define the structure of a proposal and contract-wide storage respectively.
The former contains fields that are required to submit a proposal.
The latter keeps global information like information about accepted proposals.
They can contain any fields that can theoretically appear in storage; the only exception is that `proposalMetadata` cannot contain unpackable data like `big_map`s, since we need to be able to compute hash of every proposal.
Here is an example of "treasury" style DAO:
```haskell
data ProposalMetadata = ProposalMetadata
  { mutezAmount :: Mutez
  , recipientId :: Text
  , agoraPostId :: Natural
  ...
  }

type ContractExtra = ()  -- empty
```

Compile-time value parameters are captured by the `Config` type (parameterized by `proposalMetadata` and `contractExtra`):

```haskell
data Config contractExtra proposalMetadata = Config
  { daoName :: Text
  -- ^ Name of the DAO.
  -- Only used in documentation generator and does not get stored in @Storage@.
  , daoDescription :: Text
  -- ^ Description of the DAO.
  -- Only used in documentation generator and does not get stored in @Storage@.
  , proposalCheck :: Lambda
      (ProposeParams proposalMetadata, Storage contractExtra proposalMetadata) Bool
  -- ^ A lambda used to verify whether a proposal can be submitted.
  -- It checks 2 things: the proposal itself and the amount of tokens frozen upon submission.
  -- It allows the DAO to reject a proposal by arbitrary logic and captures bond requirements
  , rejectedProposalReturnValue :: Lambda
      (Proposal proposalMetadata, Storage contractExtra proposalMetadata) Natural
  -- ^ When a proposal is rejected, the value that voters get back can be slashed.
  -- This lambda specifies how many tokens will be received.
  -- For example, if Alice freezes 100 tokens to vote and this lambda divides the value by 2,
  -- only 50 tokens will be unfrozen and other 50 tokens will be burnt.
  , decisionLambda :: Lambda
      (Proposal proposalMetadata, Storage contractExtra proposalMetadata)
      (List Operation, Storage contractExtra proposalMetadata)
  -- ^ The decision lambda is executed based on a successful proposal.
  -- It has access to the proposal, can modify `contractExtra` and perform arbitrary
  -- operations.
  -- In case if some data needs to be gathered from the accepted proposal, we suggest
  -- adding a `BigMap ProposalKey data` to `contractExtra` and put the data there.
  , maxProposals :: Natural
  -- ^ Determine the maximum number of ongoing proposals that are allowed in the contract.
  , maxVotes :: Natural
  -- ^ Determine the maximum number of votes associated with a proposal including positive votes
  -- and negative votes.
  , maxQuorumThreshold :: Natural
  -- ^ Determine the maximum value of quorum threshold that is allowed to be set.
  , minQuorumThreshold :: Natural
  -- ^ Determine the minimum value of quorum threshold that is allowed to be set.
  , maxVotingPeriod :: Natural
  -- ^ Determine the maximum value of voting period that is allowed to be set.
  , minVotingPeriod :: Natural
  -- ^ Determine the minimum value of voting period that is allowed to be set.
  }
```

The `FA2Metadata` type matches `token_metadata` defined in FA2.
The `Proposal` type is defined below.
`Storage` is the full storage type of the contract.

```haskell
type ProposalKey = ByteString

data Proposal proposalMetadata = Proposal
  { pUpvotes :: Natural
  , pDownvotes :: Natural
  , pStartDate :: Timestamp
  , pMetadata :: proposalMetadata
  , pProposer :: Address
  , pProposerFrozenToken :: Natural
  , pVoters :: [(Address, Natural)]
  -- ^ List of voter addresses associated with the vote amount
  -- Needed for `flush` entrypoint.
  }
```
## Runtime configuration

Some configuration values are specified in runtime and can be changed during the contract lifetime.
They must be provided on origination to construct the contract's initial storage.
These values are:
1. `admin :: Address` is the address that can perform administrative actions.
2. `votingPeriod :: Natural` specifies how long the voting period lasts.
3. `quorumThreshold :: Natural` specifies how many total votes are required for a successful proposal.

# Contract logic

This chapter provides a high-level overview of the contract's logic.

- The contract maintains a ledger of address and its balance (frozen and unfrozen tokens)
- The contract manages a special role called "Administrator".
- The contract stores a list of proposals that can be in one of the states: "ongoing", "rejected", or "accepted".
- Migration forms a two-step process:
  + After the migration starts, the contract `migrationStatus` state is set to `MigratingTo targetAddress` state.
  + After successful migration confirmation, `migrationStatus` is set to `MigratedTo newContractAddress` state. All the subsequent operations, except `transfer_contract_tokens`, will fail with `MIGRATED` tag paired with the updated contract address.
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

Proposals are identified by a key which is a bytestring computed via Blake2B hashing function of a
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

<!--
# Storage

The storage must be a nested tree of `pair`s with field annotations.
It must contain the following fields:
-->

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
| `OUT_OF_BOUND_VOTING_PERIOD`    | Throws when trying to set voting period that is out of bound from what is specified in the `Config`         |
| `OUT_OF_BOUND_QUORUM_THRESHOLD` | Throws when trying to set quorum threshold that is out of bound from what is specified in the `Config`      |
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
* [`get_vote_permit_counter`](#get_vote_permit_counter)
* [`get_total_supply`](#get_total_supply)

Format:
```
**entrypoint_name**

<optional Haskell definition of the argument type>
Parameter (in Michelson): X

<description>
```

* Top-level contract parameter type MUST have all entrypoints listed below.
* Each entrypoint MUST be callable using the standard entrypoints machinery of Michelson by specifying **entrypoint_name** and a value of the type `X` (its argument).
* The previous bullet point implies that each `X` must have a field annotations with the corresponding entrypoint name.
In the definitions below it may be omitted, but it is still implied.

Note: Haskell definitions are provided only for readability.
If Michelson type contradicts what's written in Haskell definition, the Michelson definition takes precedence.

## Standard FA2 Token Functions

Functions present in the [*FA2 Tezos Token Standard*][FA2].

### **transfer**

```haskell
type TokenId = Natural

data TransferDestination = TransferDestination
  { to_ :: Address
  , token_id :: TokenId
  , nat :: Natural
  }

data TransferParam = TransferParam
  { from_ :: Address
  , txs :: [TransferDestination]
  }

type TransferParams = [TransferParam]

data Parameter proposalMetadata otherParam
  = Transfer TransferParam
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

```haskell
type TokenId = Natural

data BalanceRequestItem = BalanceRequestItem
  { owner :: Address
  , token_id :: TokenId
  }

data BalanceResponseItem = BalanceResponseItem
  { request :: BalanceRequestItem
  , balance :: Natural
  }

type BalanceRequestParams =
  View [BalanceRequestItem] [BalanceResponseItem]

data Parameter proposalMetadata otherParam
  = Balance_of BalanceRequestParams
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

### **update_operators**

```haskell
type TokenId = Natural

data OperatorParam = OperatorParam
  { owner :: Address
  , operator :: Address
  , token_id :: TokenId
  }

data UpdateOperator
  = Add_operator OperatorParam
  | Remove_operator OperatorParam

type UpdateOperatorsParam = [UpdateOperator]

data Parameter
  = Update_operators UpdateOperatorsParam
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

```haskell
data MintParam = MintParam
  { to_ :: Address
  , amount :: Natural
  , token_id :: TokenId
  }

data Parameter proposalMetadata otherParam
  = Mint MintParam
```

Parameter (in Michelson):
```
(pair
  (address %to_)
  (pair
    (nat %amount)
    (nat %token_id)
  )
)
```

- Produces the given amounts of tokens to the wallet associated with the given address.
- Fails with `NOT_ADMIN` if the sender is not the administrator.

### **burn**

```haskell
data BurnParam = BurnParam
  { from :: Address
  , amount :: Natural
  , token_id :: TokenId
  }

data Parameter proposalMetadata otherParam
  = Burn BurnParam
```

Parameter (in Michelson):
```
(pair
  (address %from)
  (pair
    (nat %amount)
    (nat %token_id)
  )
)
```

- Reduce the given amounts of tokens to the wallet associated with the given address.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `FA2_INSUFFICIENT_BALANCE` if the wallet associated with the given address
does not have enough tokens to burn.

### **transfer_contract_tokens**

```haskell
type TokenId = Natural

data TransferDestination = TransferDestination
  { to_ :: Address
  , token_id :: TokenId
  , nat :: Natural
  }

data TransferParam = TransferParam
  { from_ :: Address
  , txs :: [TransferDestination]
  }

type TransferParams = [TransferParam]

data Parameter proposalMetadata otherParam
  = Transfer_contract_tokens Address TransferParams
```

Parameter (in Michelson):
```
(pair %transfer_contract_tokens
  address
  (list
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
)
```

- This entrypoint can be used by the administrator to transfer tokens owned (or operated) by this contract in another FA2 contract.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- If the outermost address passed to this entrypoint is a smart contract with FA2 `transfer` entrypoint, this entrypoint is called with supplied argument.
That is, the list of operations returned from the baseDAO contract should contain one `TRANSFER_TOKENS` operation calling the `transfer` entrypoint.
Otherwise the call fails.

## Role reassigning functions

### **transfer_ownership**

```haskell
type TransferOwnershipParam = Address

data Parameter proposalMetadata otherParam
  = Transfer_ownership TransferOwnershipParam
```

Parameter (in Michelson):
```
address
```

- Initiate transfer of the role of administrator to a new address.

- Fails with `NOT_ADMIN` if the sender is not the administrator.

- The current administrator retains his privileges up until
  `accept_ownership` is called.

- Can be called multiple times, each call replaces pending administrator with
  the new one. Note, that if proposed administrator is the same as the current
  one, then the pending administrator is simply invalidated.

### **accept_ownership**

```haskell
data Parameter proposalMetadata otherParam
  = Accept_ownership
```

Parameter (in Michelson):
```
unit
```

- Accept the administrator privilege.

- Fails with `NOT_PENDING_ADMIN` if the sender is not the current pending administrator, this also includes the case when pending administrator was not set.

- When pending administrator is not set, it is considered equal to the current owner, thus administrator can accept ownership of its own contract without a prior `transfer_ownership` call.

## Proposal entrypoints

### **propose**

```haskell
data ProposeParams proposalMetadata = ProposeParams
  { proposalTokenAmount :: Natural
  --  ^ Determines how many sender's tokens will be frozen to get
  -- the proposal accepted
  , proposalMetadata :: proposalMetadata
  }

data Parameter proposalMetadata otherParam
  = Propose (ProposeParams proposalMetadata)
```

Parameter (in Michelson):
```
(pair %propose
  (<proposal_type> %metadata)
  (nat %proposal_token_amount)
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

```haskell
-- | Voting period in seconds
type VotingPeriod = Natural

data Parameter proposalMetadata otherParam
  = Set_voting_period VotingPeriod
```

Parameter (in Michelson):
```
nat
```

- Update how long the voting period should last.
- This affects all ongoing and new proposals.
- Voting period value is measured in seconds.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `OUT_OF_BOUND_VOTING_PERIOD` if the voting period value is out of the bound set by the configuration

### **set_quorum_threshold**

```haskell
-- | QuorumThreshold that a proposal need to meet
-- quorum_threshold = upvote + downvote
type QuorumThreshold = Natural

data Parameter proposalMetadata otherParam
  = Set_quorum_threshold QuorumThreshold
```

Parameter (in Michelson):
```
nat
```

- Update the quorum threshold value which proposals have to met to not get rejected.
- Quorum threshold value is calculated by adding the number of upvotes with downvotes.
- This affects all ongoing and new proposals.
- Fails with `NOT_ADMIN` if the sender is not the administrator.
- Fails with `OUT_OF_BOUND_QUORUM_THRESHOLD` if the voting period value is out of the bound set by the configuration

### **vote**

```haskell
type ProposalKey = ByteString

type VoteType = Bool

data VoteParam = VoteParam
  { proposalKey :: ProposalKey
  , voteType :: Bool
  , voteAmount :: Natural
  }

data Permit = Permit
  { key :: PublicKey
  , signature :: Signature
  }

data Parameter proposalMetadata otherParam
  = Vote [(VoteParam, Maybe Permit)]
```

Parameter (in Michelson):

```
(list %vote
  (pair :permit_protected
    (pair
      (nat %proposal_key)
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

- This implements permits mechanism similar to the one in TZIP-017 but injected directly to the entrypoint.
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

```haskell
data Parameter proposalMetadata otherParam
  = Flush Natural
```

Parameter (in Michelson):
```
nat
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

```haskell
data Parameter proposalMetadata
  = Drop_proposal ProposalKey
```

Parameter (in Michelson):
```
bytes
```

- Delete a finished and accepted proposal that is not flushed. Tokens frozen for this
proposal are returned to the proposer and voters in full. The decision lambda is skipped.
- This entrypoint should only be used when there is a proposal that is stuck due to having a
failing decision lambda.
- Fails with `NOT_ADMIN` if the sender is not the administrator.

## Migrating functions

### **migrate**

```haskell
data Parameter proposalMetadata otherParam
  = Migrate Address
```

Parameter (in Michelson):
```
address
```

- After a successful `migrate` call, the contract will be in a state where it
  will accept a `confrimMigration` call from the new contract address. Until this
  call is received, the contract is not considered to be migrated, and will
  continue normal operations.
- The address denotes the new DAO address.
- Overwrites the new address from any previous `migrate` calls.
- Fails with `NOT_ADMIN` if the sender is not the administrator.

### **confirm_migration**

```haskell
data Parameter proposalMetadata otherParam
  = Confirm_migration ()
```

Parameter (in Michelson):
```
unit
```

- After a successful `confirm_migration` call, the contract is permanently set
  to the migrated state, no operations are possible. All contract calls, except
  `transfer_contract_tokens`, fail with `("MIGRATED", KT1NewDAO)` pair where
  `KT1NewDAO` is the address that was passed to `migrate`, ie the address of the
  new version. The `transfer_contract_tokens` entrypoint will continue to work
  even after migration is completed.
- Fails with `NOT_MIGRATING` if `migrate` operation has not been called yet.
- Fails with `NOT_MIGRATION_TARGET` if the sender of this call is not the address of the new version.

[FA2]: https://gitlab.com/tzip/tzip/-/blob/3a6464b1e641008b77a83807a0c102e7602c6af4/proposals/tzip-12/tzip-12.md

## Views

### **get_vote_permit_counter**

```haskell
data Parameter proposalMetadata otherParam
  = Get_vote_permit_counter (Void () Natural)
```

Parameter (in Michelson):
```
(pair %get_vote_permit_counter
  (unit %voidParam)
  (lambda %voidResProxy nat nat)
)
```

- A `void` entrypoint as defined in [TZIP-004](https://gitlab.com/tzip/tzip/-/blob/23c5640db0e2242878b4f2dfacf159a5f6d2544e/proposals/tzip-4/tzip-4.md#void-entrypoints).
- For `vote` entrypoint with permit, returns the current suitable counter for constructing permit signature.

### **get_total_supply**

```haskell
data Parameter proposalMetadata otherParam
  = Get_total_supply (Void TokenId Natural)
```

Parameter (in Michelson):
```
(pair %get_total_supply
  (nat %voidParam)
  (contract %voidResProxy nat)
)
```

- A `void` entrypoint as defined in [TZIP-004](https://gitlab.com/tzip/tzip/-/blob/23c5640db0e2242878b4f2dfacf159a5f6d2544e/proposals/tzip-4/tzip-4.md#void-entrypoints).
- Return the total number of tokens for the given token id.
- Fail with `FA2_TOKEN_UNDEFINED` if the given token id is not equal to `0` or `1`.

## Custom entrypoints

BaseDAO allows DAOs to define their own additional entrypoints.

```haskell
data Parameter proposalMetadata otherParam
  = CallCustom otherParam
```

By default, no custom entrypoints are defined (call of `CallCustom` will fail; in Edo it won't be callable at all).

DAO developer can provide arbitrary entrypoints that will be callable by their names.
* For instance, TreasuryDAO may define `data OtherParam = Default ()` entrypoint that will be used to provide mutez to the contract.

# TZIP-016 metadata

Note: we do not provide a proper support for contract metadata until [the proposal](https://gitlab.com/tzip/tzip/-/merge_requests/94) is finalized and merged.

This contract implements TZIP-016.

The DAO contract itself won't store the metadata, rather a dedicated contract will contain that.
Motivation:
* The baseDAO contract is very large and approaches the Tezos hard limits.
  * Providing the metadata during origination would significantly reduce the amount of functionality we can put into contract;
  * Inserting the metadata after origination still requires mechanisms to manage the metadata, increasing the contract size.

## Deployment with metadata

The deployment of contract with metadata has an extra step: a dedicated contract for carrying metadata has to be originated first.
Then the baseDAO contract should include the reference to a metadata key in the contract in order to be compliant with TZIP-016.
