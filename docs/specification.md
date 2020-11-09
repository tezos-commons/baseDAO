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

BaseDAO has 1 type parameter `proposalMetadata` that defines the structure of a proposal.
It contains fields that are required to submit a proposal.
It can contain any fields.
Here is an example of "treasury" style DAO:
```haskell
data ProposalMetadata = ProposalMetadata
  { mutezAmount :: Mutez
  , recipientId :: Text
  , agoraPostId :: Natural
  ...
  }
```

Compile-time value parameters are captured by the `Config` type (parameterized by `proposalMetadata`):

```haskell
data Config proposalMetadata = Config
  { daoName :: Text
  -- ^ Name of the DAO.
  , daoDescription :: Text
  -- ^ Description of the DAO.
  , unfrozenTokenMetadata :: FA2.TokenMetadata
  -- ^ FA2 metadata for unfrozen token.
  , frozenTokenMetadata :: FA2.TokenMetadata
  -- ^ FA2 metadata for frozen token.
  , proposalCheck :: Lambda (ProposeParams proposalMetadata) Bool
  -- ^ A lambda used to verify whether a proposal can be submitted.
  -- It checks 2 things: the proposal itself and the amount of tokens frozen upon submission.
  -- It allows the DAO to reject a proposal by arbitrary logic and captures bond requirements
  , rejectedProposalReturnValue :: Lambda (Proposal proposalMetadata) Natural
  -- ^ When a proposal is rejected, the value that voters get back can be slashed.
  -- This lambda specifies how many tokens will be received.
  -- For example, if Alice freezes 100 tokens to vote and this lambda divides the value by 2,
  -- only 50 tokens will be unfrozen and other 50 tokens will be burnt.
  , decisionLambda :: Lambda
      (Proposal proposalMetadata, Storage proposalMetadata)
      (List Operation, Storage proposalMetadata)
  -- ^ The decision lambda is executed based on a successful proposal.
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
- Migration form a two-step process:
  + After migration start the contract `migrationStatus` state is set to `MigratingTo targetAddress` state.
  + After successful migration confirmation `migrationStatus` is set to `MigratedTo newContractAddress` state. All the subsequent operations, except `transfer_contract_tokens`, will fail with `MIGRATED` tag paired with the updated contract address.
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

Proposals are identified by a key which is a bytestring computed via Blake2B hasing function of a
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

| Error                          | Description                                            |
|--------------------------------|--------------------------------------------------------|
| `NOT_ADMIN`            | The sender is not the administrator                          |
| `NOT_PENDING_ADMINISTRATOR`    | Authorized sender is not the current pending administrator   |
| `NO_PENDING_ADMINISTRATOR_SET` | Throws when trying to authorize as the pending administrator whilst is not set for a contract |
| `NOT_TOKEN_OWNER`              | Trying to configure operators for a different wallet which sender does not own                |
| `FAIL_TRANSFER_CONTRACT_TOKENS` | Trying to cross-transfer DAO tokens to another contract that does not exist or is not a valid FA2 contract. |
| `FAIL_PROPOSAL_CHECK`          | Throws when trying to propose a proposal that does not pass `proposalCheck`               |
| `FROZEN_TOKEN_NOT_TRANSFERABLE`| Transfer entrypoint is called for frozen token by a non-admin sender|
| `PROPOSAL_INSUFFICIENT_BALANCE`| Throws when trying to propose a proposal without having enough unfrozen token                |
| `VOTING_INSUFFICIENT_BALANCE`  | Throws when trying to vote on a proposal without having enough unfrozen token                |
| `PROPOSAL_NOT_EXIST`           | Throws when trying to vote on a proposal that does not exist |
| `QUORUM_NOT_MET`               | A proposal is flushed, but there are not enough votes        |
| `VOTING_PERIOD_OVER`           | Throws when trying to vote on a proposal that is already ended        |
| `OUT_OF_BOUND_VOTING_PERIOD`   | Throws when trying to set voting period that is out of bound from what is specified in the `Config`        |
| `OUT_OF_BOUND_QUORUM_THRESHOLD`| Throws when trying to set quorum threshold that is out of bound from what is specified in the `Config`        |
| `MAX_PROPOSALS_REACHED`        | Throws when trying to propose a proposal when proposals max amount is already reached |
| `MAX_VOTES_REACHED`            | Throws when trying to vote on a proposal when the votes max amount of that proposal is already reached |
| `CONTRACT_MIGRATED`            | Throw when conract has been migrated        |
| `MIGRATED`                     | Throw when conract has been migrated        |
| `NOT_MIGRATING`                | Throw when `confirm_migration` is called and contract is not in migration |
| `NOT_MIGRATION_TARGET`         | Throw when `confirm_migration` is called by address other than the new address |
| `FORBIDDEN_XTZ`                | Throws when some XTZ was received as part of the contract call |
| `PROPOSER_NOT_EXIST_IN_LEDGER` | Expect a proposer address to exist in Ledger but it is not found         |
| `PROPOSAL_NOT_UNIQUE`          | Trying to propose a proposal that is already existed in the Storage.         |


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
* [`token_address`](#token_address)
* [`flush`](#flush)
* [`migrate`](#migrate)
* [`confirm_migration`](#confirm_migration)

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

data Parameter proposalMetadata
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

data Parameter proposalMetadata
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

### **token_metadata_registry**

```haskell
type TokenMetadataRegistryParam = ContractRef Address

data Parameter proposalMetadata
  = Token_metadata_registry TokenMetadataRegistryParam
```
Parameter (in Michelson)
```
(contract %token_metadata_registry address)
```

- Return contract address that holds token metadata.
- Token metadata MUST include the DAO metadata.

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

data Parameter proposalMetadata
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

data Parameter proposalMetadata
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

data Parameter proposalMetadata
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

data Parameter proposalMetadata
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
data Parameter proposalMetadata
  = Accept_ownership
```

Parameter (in Michelson):
```
unit
```

- Accept the administrator privilege.

- Fails with `NOT_PENDING_ADMINISTRATOR` if the sender is not the current pending administrator or `NO_PENDING_ADMINISTRATOR_SET` if there is no pending administrator.

## Proposal entrypoints

### **propose**

```haskell
data ProposeParams proposalMetadata = ProposeParams
  { proposalTokenAmount :: Natural
  --  ^ Determines how many sender's tokens will be frozen to get
  -- the proposal accepted
  , proposalMetadata :: proposalMetadata
  }

data Parameter proposalMetadata
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

data Parameter proposalMetadata
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

data Parameter proposalMetadata
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

data Parameter proposalMetadata
  = Vote [VoteParam]
```

Parameter (in Michelson):

```
(list
  (pair %vote
    (nat %proposal_key)
    (pair
      (bool %vote_type)
      (nat %vote_amount)
    )
  )
)
```

- Sender MUST have unfrozen tokens equal to `voteAmount` or more (1 unfrozen token is needed for 1 vote).
- Fails with `VOTING_INSUFFICIENT_BALANCE` if the unfrozen token balance of the SENDER
  is less than specified `vVoteAmount` .
- Fails with `PROPOSAL_NOT_EXIST` if the proposal key is not associated with any ongoing proposals.
- Fails with `VOTING_PERIOD_OVER` if the proposal associated with the proposal key is already ended.
- Fails with `MAX_VOTES_REACHED` if the amount of votes of the associated proposal is already at the max value set by the configuration.
- The sender's balance in frozen tokens is increased by `voteAmount` and in unfrozen tokens is decreased by `voteAmount`.
- The entrypoint accepts a list of vote params. As a result, the sender can `vote` on multiple proposals (or the same proposal multiple time) in one entrypoint call.

### **token_address**

```haskell
data Parameter proposalMetadata
  = Token_address (ContractRef Address)
```

Parameter (in Michelson)
```
(contract %token_address address)
```

- Return the address of the associated FA2 contract.
- Since FA2 logic is embedded into this contract, this entrypoint always returns `SELF`.

### **flush**

```haskell
data Parameter proposalMetadata
  = Flush ()
```

Parameter (in Michelson):
```
unit
```

- Finish voting process on all proposals for which voting period is over.
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

## Migrating functions

### **migrate**

```haskell
data Parameter proposalMetadata
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
data Parameter proposalMetadata
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
