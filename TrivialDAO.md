# BaseDAO

**Code revision:** [c92d29d](https://github.com/tqtezos/baseDAO/tree/c92d29dcf064df6512fd2867b9c14044e573d582) *(Wed Dec 9 15:50:52 2020 +0300)*



An example of a very simple DAO contract without any custom checks,
                          extra data and decision lambda.

It contains standard FA2 entrypoints, plus some extra ones including proposal and
migration entrypoints. It supports two types of token_id - frozen (token_id = 1) and unfrozen (token_id = 0).


## Table of contents

- [Haskell ⇄ Michelson conversion](#haskell-⇄-Michelson-conversion)
- [Storage](#storage)
  - [Storage](#storage-Storage)
- [Entrypoints](#entrypoints)
  - [call_FA2](#entrypoints-call_FA2)
  - [transfer_ownership](#entrypoints-transfer_ownership)
  - [accept_ownership](#entrypoints-accept_ownership)
  - [migrate](#entrypoints-migrate)
  - [confirm_migration](#entrypoints-confirm_migration)
  - [propose](#entrypoints-propose)
  - [vote](#entrypoints-vote)
  - [set_voting_period](#entrypoints-set_voting_period)
  - [set_quorum_threshold](#entrypoints-set_quorum_threshold)
  - [flush](#entrypoints-flush)
  - [burn](#entrypoints-burn)
  - [mint](#entrypoints-mint)
  - [transfer_contract_tokens](#entrypoints-transfer_contract_tokens)
  - [token_address](#entrypoints-token_address)
- [Possible errors](#possible-errors)

**[Definitions](#definitions)**

- [Types](#types)
  - [()](#types-lparenrparen)
  - [(a, b)](#types-lparenacomma-brparen)
  - [Address (no entrypoint)](#types-Address-lparenno-entrypointrparen)
  - [BalanceRequestItem](#types-BalanceRequestItem)
  - [BalanceResponseItem](#types-BalanceResponseItem)
  - [BigMap](#types-BigMap)
  - [Bool](#types-Bool)
  - [BurnParam](#types-BurnParam)
  - [ByteString](#types-ByteString)
  - [Contract](#types-Contract)
  - [Integer](#types-Integer)
  - [List](#types-List)
  - [MigrationStatus](#types-MigrationStatus)
  - [MintParam](#types-MintParam)
  - [Named entry](#types-Named-entry)
  - [Natural](#types-Natural)
  - [OperatorParam](#types-OperatorParam)
  - [Parameter](#types-Parameter)
  - [Proposal](#types-Proposal)
  - [ProposeParams](#types-ProposeParams)
  - [Text](#types-Text)
  - [Timestamp](#types-Timestamp)
  - [TransferContractTokensParam](#types-TransferContractTokensParam)
  - [TransferDestination](#types-TransferDestination)
  - [TransferItem](#types-TransferItem)
  - [UpdateOperator](#types-UpdateOperator)
  - [View](#types-View)
  - [VoteParam](#types-VoteParam)
- [Errors](#errors)
  - [FA2_INSUFFICIENT_BALANCE](#errors-FA2_INSUFFICIENT_BALANCE)
  - [FA2_NOT_OPERATOR](#errors-FA2_NOT_OPERATOR)
  - [FA2_TOKEN_UNDEFINED](#errors-FA2_TOKEN_UNDEFINED)
  - [FAIL_PROPOSAL_CHECK](#errors-FAIL_PROPOSAL_CHECK)
  - [FAIL_TRANSFER_CONTRACT_TOKENS](#errors-FAIL_TRANSFER_CONTRACT_TOKENS)
  - [FORBIDDEN_XTZ](#errors-FORBIDDEN_XTZ)
  - [FROZEN_TOKEN_NOT_TRANSFERABLE](#errors-FROZEN_TOKEN_NOT_TRANSFERABLE)
  - [InternalError](#errors-InternalError)
  - [MAX_PROPOSALS_REACHED](#errors-MAX_PROPOSALS_REACHED)
  - [MAX_VOTES_REACHED](#errors-MAX_VOTES_REACHED)
  - [MIGRATED](#errors-MIGRATED)
  - [NOT_ADMIN](#errors-NOT_ADMIN)
  - [NOT_MIGRATING](#errors-NOT_MIGRATING)
  - [NOT_MIGRATION_TARGET](#errors-NOT_MIGRATION_TARGET)
  - [NOT_OWNER](#errors-NOT_OWNER)
  - [NOT_PENDING_ADMIN](#errors-NOT_PENDING_ADMIN)
  - [OUT_OF_BOUND_QUORUM_THRESHOLD](#errors-OUT_OF_BOUND_QUORUM_THRESHOLD)
  - [OUT_OF_BOUND_VOTING_PERIOD](#errors-OUT_OF_BOUND_VOTING_PERIOD)
  - [PROPOSAL_INSUFFICIENT_BALANCE](#errors-PROPOSAL_INSUFFICIENT_BALANCE)
  - [PROPOSAL_NOT_EXIST](#errors-PROPOSAL_NOT_EXIST)
  - [PROPOSAL_NOT_UNIQUE](#errors-PROPOSAL_NOT_UNIQUE)
  - [PROPOSER_NOT_EXIST_IN_LEDGER](#errors-PROPOSER_NOT_EXIST_IN_LEDGER)
  - [VOTING_INSUFFICIENT_BALANCE](#errors-VOTING_INSUFFICIENT_BALANCE)
  - [VOTING_PERIOD_OVER](#errors-VOTING_PERIOD_OVER)



## Haskell ⇄ Michelson conversion

This smart contract is developed in Haskell using the [Morley framework](https://gitlab.com/morley-framework/morley). Documentation mentions Haskell types that can be used for interaction with this contract from Haskell, but for each Haskell type we also mention its Michelson representation to make interactions outside of Haskell possible.

There are multiple ways to interact with this contract:

* Use this contract in your Haskell application, thus all operation submissionshould be handled separately, e.g. via calling `tezos-client`, which will communicate with the `tezos-node`. In order to be able to call `tezos-client` you'll need to be able to construct Michelson values from Haskell.

  The easiest way to do that is to serialize Haskell value using `lPackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs) module, encode resulting bytestring to hexadecimal representation using `encodeHex` function. Resulting hexadecimal encoded bytes sequence can be decoded back to Michelson value via `tezos-client unpack michelson data`.

  Reverse conversion from Michelson value to the Haskell value can be done by serializing Michelson value using `tezos-client hash data` command, resulting `Raw packed data` should be decoded from the hexadecimal representation using `decodeHex` and deserialized to the Haskell value via `lUnpackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs).

* Contruct values for this contract directly on Michelson level using types provided in the documentation.

## Storage

<a name="storage-Storage"></a>

---

### `Storage`

Storage type for baseDAO contract

**Structure (example):** `Storage Natural MText` = 
  * ***sLedger*** :[`BigMap`](#types-BigMap) ([`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), [`Natural`](#types-Natural)) [`Natural`](#types-Natural)
  * ***sOperators*** :[`BigMap`](#types-BigMap) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***operator*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`()`](#types-lparenrparen)
  * ***sTokenAddress*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***sAdmin*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***sPendingOwner*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***sMigrationStatus*** :[`MigrationStatus`](#types-MigrationStatus)
  * ***sVotingPeriod*** :[`Natural`](#types-Natural)
  * ***sQuorumThreshold*** :[`Natural`](#types-Natural)
  * ***sExtra*** :[`Natural`](#types-Natural)
  * ***sProposals*** :[`BigMap`](#types-BigMap) [`ByteString`](#types-ByteString) ([`Proposal`](#types-Proposal) [`Text`](#types-Text))
  * ***sProposalKeyListSortByDate*** :[`List`](#types-List) [`ByteString`](#types-ByteString)

**Final Michelson representation (example):** `Storage Natural MText` = `pair (pair (pair (big_map (pair address nat) nat) (big_map (pair address address) unit)) (pair address (pair address address))) (pair (pair (or unit (or address address)) (pair nat nat)) (pair nat (pair (big_map bytes (pair (pair nat (pair nat timestamp)) (pair (pair string address) (pair nat (list (pair address nat)))))) (list bytes))))`



## Entrypoints

<a name="entrypoints-call_FA2"></a>

---

### `call_FA2`

Entrypoint to be called if you want to use one of FA2 entrypoints.


**Argument:** 
  + **In Haskell:** [`Parameter`](#types-Parameter)
  + **In Michelson:** `(or (or (list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))) (pair (list %requests (pair (address %owner) (nat %token_id))) (contract %callback (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))) (or (contract address) (list (or (pair %add_operator (address %owner) (pair (address %operator) (nat %token_id))) (pair %remove_operator (address %owner) (pair (address %operator) (nat %token_id)))))))`
    + **Example:** <span id="example-id">`Left (Left { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0) } })`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `call_FA2` entrypoint passing the constructed argument.
</details>
<p>



#### FA2 entrypoints

---

##### `transfer`

Transfer tokens between a given account and each account from the given list.

It serves multiple purposes:
* If transaction `"from"` address equals to the admin address presented in storage, then  it is allowed for
any address and both `frozen` and `unfrozen` tokens
* Otherwise, it is allowed to transfer money only if `from` address equals to sender address or have sender as an operator
It is also prohibited to send frozen tokens in this case.


**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`TransferItem`](#types-TransferItem)
  + **In Michelson:** `(list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount))))))`
    + **Example:** <span id="example-id">`{ Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0) } }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transfer` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`FA2_NOT_OPERATOR`](#errors-FA2_NOT_OPERATOR) — The sender of transfer is not the owner or the authorized operator

* [`FROZEN_TOKEN_NOT_TRANSFERABLE`](#errors-FROZEN_TOKEN_NOT_TRANSFERABLE) — The sender tries to transfer frozen token

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens



---

##### `balance_of`

Returns the balance of specified address in ledger.
The entrypoint supports both frozen and unfrozen tokens.


**Argument:** 
  + **In Haskell:** [`View`](#types-View) ([`List`](#types-List) [`BalanceRequestItem`](#types-BalanceRequestItem)) ([`List`](#types-List) [`BalanceResponseItem`](#types-BalanceResponseItem))
  + **In Michelson:** `(pair (list %requests (pair (address %owner) (nat %token_id))) (contract %callback (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))`
    + **Example:** <span id="example-id">`Pair { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0 } "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `balance_of` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id



---

##### `token_metadata_registry`

Returns contract address that holds token metadata.
Token metadata will contain the DAO metadata.


**Argument:** 
  + **In Haskell:** [`ContractRef`](#types-Contract) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(contract address)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `token_metadata_registry` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract



---

##### `update_operators`

Updates operators. There are 2 different opportunities:

* Add operator - updates operators with a new pair of `(operator, owner)`.
If this pair already exists, this entrypoint does nothing
* Remove operator - updates operators, removing the existing pair of `(operator, owner)`.
If there is no such key, this entrypoint does nothing.

All tokens passed to this entrypoint as a part of an argument must be unfrozen.
Each owner must be equal to sender, or the entrypoint fails.


**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`UpdateOperator`](#types-UpdateOperator)
  + **In Michelson:** `(list (or (pair %add_operator (address %owner) (pair (address %operator) (nat %token_id))) (pair %remove_operator (address %owner) (pair (address %operator) (nat %token_id)))))`
    + **Example:** <span id="example-id">`{ Left (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0)) }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `update_operators` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NOT_OWNER`](#errors-NOT_OWNER) — The sender of transaction is not owner





<a name="entrypoints-transfer_ownership"></a>

---

### `transfer_ownership`

Asks an Address to become an admin. Can be called only by current administrator. The admin duties transfer only when the
requested address accepts ownership. If called multiple times, only the last called address can accept ownership.


**Argument:** 
  + **In Haskell:** ***newOwner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(address :newOwner)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transfer_ownership` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator



<a name="entrypoints-accept_ownership"></a>

---

### `accept_ownership`

Accepts the administrator privelege.
Only works when the sender was asked to become an admin and only if it was asked by the current admin.


**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `accept_ownership` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `pending owner`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_PENDING_ADMIN`](#errors-NOT_PENDING_ADMIN) — Received an `accept_ownership` from an address other than what is in the pending owner field



<a name="entrypoints-migrate"></a>

---

### `migrate`

Asks an address to migrate the contract to it.
The contract is not considered migrated, until it receives confirm_migration call.


**Argument:** 
  + **In Haskell:** ***newAddress*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(address :newAddress)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `migrate` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator



<a name="entrypoints-confirm_migration"></a>

---

### `confirm_migration`

Confirms migration of a contract to the sender address.
After a successful call the contract will be set to migrated state where no operations are possible.


**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `confirm_migration` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `migration target contract`.

**Possible errors:**
* [`NOT_MIGRATING`](#errors-NOT_MIGRATING) — Recieved a confirm_migration call on a contract that is not in migration

* [`NOT_MIGRATION_TARGET`](#errors-NOT_MIGRATION_TARGET) — Recieved a confirm_migration call on a contract from an address other than the new version

* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract



<a name="entrypoints-propose"></a>

---

### `propose`

Saves the proposal with specific id and freezes the amount of sender tokens equal to the given amount.
The sender must have enough unfrozen tokens.
The sender's amount of frozen tokens is increased by proposal ammount. And the amount of unfrozen one
is decreased by the same value.


**Argument:** 
  + **In Haskell:** [`ProposeParams`](#types-ProposeParams) [`()`](#types-lparenrparen)
  + **In Michelson:** `(pair (nat %frozen_token) (unit %proposal_metadata))`
    + **Example:** <span id="example-id">`Pair 0 Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `propose` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`FAIL_PROPOSAL_CHECK`](#errors-FAIL_PROPOSAL_CHECK) — Trying to propose a proposal that does not pass `proposalCheck`

* [`MAX_PROPOSALS_REACHED`](#errors-MAX_PROPOSALS_REACHED) — Trying to propose a proposal when proposals max amount is already reached

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens

* [`PROPOSAL_INSUFFICIENT_BALANCE`](#errors-PROPOSAL_INSUFFICIENT_BALANCE) — Trying to propose a proposal without having enough unfrozen token

* [`PROPOSAL_NOT_UNIQUE`](#errors-PROPOSAL_NOT_UNIQUE) — Trying to propose a proposal that is already existed in the Storage.



<a name="entrypoints-vote"></a>

---

### `vote`

For each vote params in a given list vote for the proposal with that id. Thus the sender can vote many proposals
(or one proposal multiple times) in a single call.
The sender must have an amount required for all votings.


**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`VoteParam`](#types-VoteParam)
  + **In Michelson:** `(list (pair (bytes %proposal_key) (pair (bool %vote_type) (nat %vote_amount))))`
    + **Example:** <span id="example-id">`{ Pair 0x0a (Pair True 0) }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `vote` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`PROPOSAL_NOT_EXIST`](#errors-PROPOSAL_NOT_EXIST) — Trying to vote on a proposal that does not exist

* [`MAX_VOTES_REACHED`](#errors-MAX_VOTES_REACHED) — Trying to vote on a proposal when the votes max amount of that proposal is already reached

* [`VOTING_PERIOD_OVER`](#errors-VOTING_PERIOD_OVER) — Trying to vote on a proposal that is already ended

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens

* [`VOTING_INSUFFICIENT_BALANCE`](#errors-VOTING_INSUFFICIENT_BALANCE) — Trying to vote on a proposal without having enough unfrozen token



<a name="entrypoints-set_voting_period"></a>

---

### `set_voting_period`

Updates how long the voting period would last.
It affects all ongoing proposals and all created afterwards.


**Argument:** 
  + **In Haskell:** [`Natural`](#types-Natural)
  + **In Michelson:** `nat`
    + **Example:** <span id="example-id">`0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `set_voting_period` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`OUT_OF_BOUND_VOTING_PERIOD`](#errors-OUT_OF_BOUND_VOTING_PERIOD) — Trying to set voting period that is out of bound.



<a name="entrypoints-set_quorum_threshold"></a>

---

### `set_quorum_threshold`

Updates the quorum threshold with a given value.
It affects all ongoing proposals and all created afterwards.


**Argument:** 
  + **In Haskell:** [`Natural`](#types-Natural)
  + **In Michelson:** `nat`
    + **Example:** <span id="example-id">`0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `set_quorum_threshold` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`OUT_OF_BOUND_QUORUM_THRESHOLD`](#errors-OUT_OF_BOUND_QUORUM_THRESHOLD) — Trying to set quorum threshold that is out of bound



<a name="entrypoints-flush"></a>

---

### `flush`

Finish voting process on all proposals where the voting period is over.
Returns an amount to the proposer, determined by the result of voting.
There is a possibility of some tokens being lost due to administrator perform
of burn or transfer operation.
If the proposal is accepted, the decision lambda is called.


**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `flush` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`PROPOSAL_NOT_EXIST`](#errors-PROPOSAL_NOT_EXIST) — Trying to vote on a proposal that does not exist

* [`PROPOSER_NOT_EXIST_IN_LEDGER`](#errors-PROPOSER_NOT_EXIST_IN_LEDGER) — Expect a proposer address to exist in Ledger but it is not found (Impossible Case)

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens



<a name="entrypoints-burn"></a>

---

### `burn`

Reduces the amount of tokens of the given address. Can be performed only
only if the given address has enough tokens to burn.


**Argument:** 
  + **In Haskell:** [`BurnParam`](#types-BurnParam)
  + **In Michelson:** `(pair (address %from_) (pair (nat %token_id) (nat %amount)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `burn` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens



<a name="entrypoints-mint"></a>

---

### `mint`

Provides the amount of tokens of the given address.


**Argument:** 
  + **In Haskell:** [`MintParam`](#types-MintParam)
  + **In Michelson:** `(pair (address %to_) (pair (nat %token_id) (nat %amount)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `mint` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator



<a name="entrypoints-transfer_contract_tokens"></a>

---

### `transfer_contract_tokens`

This entrypoint can be used by the administrator
to transfer tokens owned (or operated) by this contract in another FA2 contract.
Unlike the others, this entrypoint can be used after contract is migrated.


**Argument:** 
  + **In Haskell:** [`TransferContractTokensParam`](#types-TransferContractTokensParam)
  + **In Michelson:** `(pair (address %contract_address) (list %params (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0) } }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transfer_contract_tokens` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`FAIL_TRANSFER_CONTRACT_TOKENS`](#errors-FAIL_TRANSFER_CONTRACT_TOKENS) — Trying to cross-transfer BaseDAO tokens to another contract that does not exist or is not a valid FA2 contract.



<a name="entrypoints-token_address"></a>

---

### `token_address`

Returns the address of the associated FA2 contract.
Since FA2 logic is embedded into this contract, this entrypoint always returns SELF.


**Argument:** 
  + **In Haskell:** [`ContractRef`](#types-Contract) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(contract address)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `token_address` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract



**Possible errors:**
* [`FORBIDDEN_XTZ`](#errors-FORBIDDEN_XTZ) — Received some XTZ as part of a contract call, which is forbidden





# Definitions

## Types

<a name="types-lparenrparen"></a>

---

### `()`

Unit primitive.

**Structure:** ()

**Final Michelson representation:** `unit`



<a name="types-lparenacomma-brparen"></a>

---

### `(a, b)`

Pair primitive.

**Final Michelson representation (example):** `(Integer,Natural)` = `pair int nat`



<a name="types-Address-lparenno-entrypointrparen"></a>

---

### `Address (no entrypoint)`

This is similar to Michelson Address, but does not retain entrypoint name if it refers to a contract.

**Final Michelson representation:** `address`



<a name="types-BalanceRequestItem"></a>

---

### `BalanceRequestItem`

Describes a request for an owner's balance

**Structure:** 
  * ***owner*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***tokenId*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address nat`



<a name="types-BalanceResponseItem"></a>

---

### `BalanceResponseItem`

Describes a response to a request for an owner's balance

**Structure:** 
  * ***request*** :[`BalanceRequestItem`](#types-BalanceRequestItem)
  * ***balance*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair (pair address nat) nat`



<a name="types-BigMap"></a>

---

### `BigMap`

BigMap primitive.

**Final Michelson representation (example):** `BigMap Integer Natural` = `big_map int nat`



<a name="types-Bool"></a>

---

### `Bool`

Bool primitive.

**Final Michelson representation:** `bool`



<a name="types-BurnParam"></a>

---

### `BurnParam`

Describes whose account, which token id and in what amount to burn

**Structure:** 
  * ***from_*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***tokenId*** :[`Natural`](#types-Natural)
  * ***amount*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address (pair nat nat)`



<a name="types-ByteString"></a>

---

### `ByteString`

Bytes primitive.

**Final Michelson representation:** `bytes`



<a name="types-Contract"></a>

---

### `Contract`

Contract primitive with given type of parameter.

**Final Michelson representation (example):** `ContractRef Integer` = `contract int`



<a name="types-Integer"></a>

---

### `Integer`

Signed number.

**Final Michelson representation:** `int`



<a name="types-List"></a>

---

### `List`

List primitive.

**Final Michelson representation (example):** `[Integer]` = `list int`



<a name="types-MigrationStatus"></a>

---

### `MigrationStatus`

Migration status of the contract

**Structure:** *one of* 
+ **NotInMigration**()
+ **MigratingTo**
[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
+ **MigratedTo**
[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)


**Final Michelson representation:** `or unit (or address address)`



<a name="types-MintParam"></a>

---

### `MintParam`

Describes whose account, which token id and in what amount to mint

**Structure:** 
  * ***to_*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***tokenId*** :[`Natural`](#types-Natural)
  * ***amount*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address (pair nat nat)`



<a name="types-Named-entry"></a>

---

### `Named entry`

Some entries have names for clarity.

In resulting Michelson names may be mapped to annotations.

**Final Michelson representation (example):** `number: Integer` = `int`



<a name="types-Natural"></a>

---

### `Natural`

Unsigned number.

**Final Michelson representation:** `nat`



<a name="types-OperatorParam"></a>

---

### `OperatorParam`

Describes an address authorized to transfer tokens on behalf of a token owner

**Structure:** 
  * ***owner*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***operator*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***tokenId*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address (pair address nat)`



<a name="types-Parameter"></a>

---

### `Parameter`

Describes the FA2 operations.

**Structure:** *one of* 
+ **Transfer**
([`List`](#types-List) [`TransferItem`](#types-TransferItem))
+ **Balance_of**
([`View`](#types-View) ([`List`](#types-List) [`BalanceRequestItem`](#types-BalanceRequestItem)) ([`List`](#types-List) [`BalanceResponseItem`](#types-BalanceResponseItem)))
+ **Token_metadata_registry**
([`ContractRef`](#types-Contract) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen))
+ **Update_operators**
([`List`](#types-List) [`UpdateOperator`](#types-UpdateOperator))


**Final Michelson representation:** `or (or (list (pair address (list (pair address (pair nat nat))))) (pair (list (pair address nat)) (contract (list (pair (pair address nat) nat))))) (or (contract address) (list (or (pair address (pair address nat)) (pair address (pair address nat)))))`



<a name="types-Proposal"></a>

---

### `Proposal`

Contract's storage holding a big_map with all balances and the operators.

**Structure (example):** `Proposal ()` = 
  * ***pUpvotes*** :[`Natural`](#types-Natural)
  * ***pDownvotes*** :[`Natural`](#types-Natural)
  * ***pStartDate*** :[`Timestamp`](#types-Timestamp)
  * ***pMetadata*** :[`()`](#types-lparenrparen)
  * ***pProposer*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***pProposerFrozenToken*** :[`Natural`](#types-Natural)
  * ***pVoters*** :[`List`](#types-List) ([`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), [`Natural`](#types-Natural))

**Final Michelson representation (example):** `Proposal ()` = `pair (pair nat (pair nat timestamp)) (pair (pair unit address) (pair nat (list (pair address nat))))`



<a name="types-ProposeParams"></a>

---

### `ProposeParams`

Describes the how many proposer's frozen tokens will be frozen and the proposal metadata

**Structure (example):** `ProposeParams ()` = 
  * ***ppFrozenToken*** :[`Natural`](#types-Natural)
  * ***ppProposalMetadata*** :[`()`](#types-lparenrparen)

**Final Michelson representation (example):** `ProposeParams ()` = `pair nat unit`



<a name="types-Text"></a>

---

### `Text`

Michelson string.

This has to contain only ASCII characters with codes from [32; 126] range; additionally, newline feed character is allowed.

**Final Michelson representation:** `string`



<a name="types-Timestamp"></a>

---

### `Timestamp`

Timestamp primitive.

**Final Michelson representation:** `timestamp`



<a name="types-TransferContractTokensParam"></a>

---

### `TransferContractTokensParam`

TODO

**Structure:** 
  * ***contractAddress*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***params*** :[`List`](#types-List) [`TransferItem`](#types-TransferItem)

**Final Michelson representation:** `pair address (list (pair address (list (pair address (pair nat nat)))))`



<a name="types-TransferDestination"></a>

---

### `TransferDestination`

Describes the amount of tokens to transfer and to whom

**Structure:** 
  * ***to*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***tokenId*** :[`Natural`](#types-Natural)
  * ***amount*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address (pair nat nat)`



<a name="types-TransferItem"></a>

---

### `TransferItem`

Describes a transfer operation

**Structure:** 
  * ***from*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***txs*** :[`List`](#types-List) [`TransferDestination`](#types-TransferDestination)

**Final Michelson representation:** `pair address (list (pair address (pair nat nat)))`



<a name="types-UpdateOperator"></a>

---

### `UpdateOperator`

Describes the operator update operation.

**Structure:** *one of* 
+ **AddOperator**
[`OperatorParam`](#types-OperatorParam)
+ **RemoveOperator**
[`OperatorParam`](#types-OperatorParam)


**Final Michelson representation:** `or (pair address (pair address nat)) (pair address (pair address nat))`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-4/tzip-4.md#view-entrypoints).

**Structure (example):** `View () Integer` = 
[`()`](#types-lparenrparen)
[`ContractRef`](#types-Contract) [`Integer`](#types-Integer)

**Final Michelson representation (example):** `View () Integer` = `pair unit (contract int)`



<a name="types-VoteParam"></a>

---

### `VoteParam`

Describes target proposal id, vote type and vote amount

**Structure:** 
  * ***proposalKey*** :[`ByteString`](#types-ByteString)
  * ***voteType*** :[`Bool`](#types-Bool)
  * ***voteAmount*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair bytes (pair bool nat)`



## Errors

Our contract implies the possibility of error scenarios, this section enlists
all values which the contract can produce via calling `FAILWITH` instruction
on them. In case of error, no changes to contract state will be applied.

Each entrypoint also contains a list of errors which can be raised during its
execution; only for no-throw entrypoints this list will be omitted.
Errors in these lists are placed in the order in which the corresponding
properties are checked unless the opposite is specified. I.e., if for a
given entrypoint call two different errors may take place, the one which
appears in the list first will be thrown.

Most of the errors are represented according to the same
`(error tag, error argument)` pattern. See the list of errors below
for details.

We distinquish several error classes:
+ **Action exception**: given action cannot be performed with
  regard to the current contract state.

  Examples: "insufficient balance", "wallet does not exist".

  If you are implementing a middleware, such errors should be propagated to
  the client.

+ **Bad argument**: invalid argument supplied to the entrypoint.

  Examples: entrypoint accepts a natural number from `0-3` range, and you
  supply `5`.

  If you are implementing a middleware, you should care about not letting
  such errors happen.

+ **Internal**: contract-internal error.

  In ideal case, such errors should not take place, but still, make sure
  that you are ready to handle them. They can signal either invalid contract
  deployment or a bug in contract implementation.

  If an internal error is thrown, please report it to the author of this contract.


<a name="errors-FA2_INSUFFICIENT_BALANCE"></a>

---

### `FA2_INSUFFICIENT_BALANCE`

**Class:** Action exception

**Fires if:** The source of a transfer did not contain sufficient tokens

**Representation:** `("FA2_INSUFFICIENT_BALANCE", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-FA2_NOT_OPERATOR"></a>

---

### `FA2_NOT_OPERATOR`

**Class:** Action exception

**Fires if:** The sender of transfer is not the owner or the authorized operator

**Representation:** `("FA2_NOT_OPERATOR", ())`.

<a name="errors-FA2_TOKEN_UNDEFINED"></a>

---

### `FA2_TOKEN_UNDEFINED`

**Class:** Action exception

**Fires if:** Contract received an unsupported token id

**Representation:** `("FA2_TOKEN_UNDEFINED", ())`.

<a name="errors-FAIL_PROPOSAL_CHECK"></a>

---

### `FAIL_PROPOSAL_CHECK`

**Class:** Action exception

**Fires if:** Trying to propose a proposal that does not pass `proposalCheck`

**Representation:** `("FAIL_PROPOSAL_CHECK", ())`.

<a name="errors-FAIL_TRANSFER_CONTRACT_TOKENS"></a>

---

### `FAIL_TRANSFER_CONTRACT_TOKENS`

**Class:** Action exception

**Fires if:** Trying to cross-transfer BaseDAO tokens to another contract that does not exist or is not a valid FA2 contract.

**Representation:** `("FAIL_TRANSFER_CONTRACT_TOKENS", ())`.

<a name="errors-FORBIDDEN_XTZ"></a>

---

### `FORBIDDEN_XTZ`

**Class:** Action exception

**Fires if:** Received some XTZ as part of a contract call, which is forbidden

**Representation:** `("FORBIDDEN_XTZ", ())`.

<a name="errors-FROZEN_TOKEN_NOT_TRANSFERABLE"></a>

---

### `FROZEN_TOKEN_NOT_TRANSFERABLE`

**Class:** Action exception

**Fires if:** The sender tries to transfer frozen token

**Representation:** `("FROZEN_TOKEN_NOT_TRANSFERABLE", ())`.

<a name="errors-InternalError"></a>

---

### `InternalError`

**Class:** Internal

**Fires if:** Some internal error occured.

**Representation:** Textual error message, see [`Text`](#types-Text).

<a name="errors-MAX_PROPOSALS_REACHED"></a>

---

### `MAX_PROPOSALS_REACHED`

**Class:** Action exception

**Fires if:** Trying to propose a proposal when proposals max amount is already reached

**Representation:** `("MAX_PROPOSALS_REACHED", ())`.

<a name="errors-MAX_VOTES_REACHED"></a>

---

### `MAX_VOTES_REACHED`

**Class:** Action exception

**Fires if:** Trying to vote on a proposal when the votes max amount of that proposal is already reached

**Representation:** `("MAX_VOTES_REACHED", ())`.

<a name="errors-MIGRATED"></a>

---

### `MIGRATED`

**Class:** Action exception

**Fires if:** Recieved a call on a migrated contract

**Representation:** `("MIGRATED", <error argument>)`.

Provided error argument will be of type [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen).

<a name="errors-NOT_ADMIN"></a>

---

### `NOT_ADMIN`

**Class:** Action exception

**Fires if:** Received an operation that require administrative privileges from an address that is not the current administrator

**Representation:** `("NOT_ADMIN", ())`.

<a name="errors-NOT_MIGRATING"></a>

---

### `NOT_MIGRATING`

**Class:** Action exception

**Fires if:** Recieved a confirm_migration call on a contract that is not in migration

**Representation:** `("NOT_MIGRATING", ())`.

<a name="errors-NOT_MIGRATION_TARGET"></a>

---

### `NOT_MIGRATION_TARGET`

**Class:** Action exception

**Fires if:** Recieved a confirm_migration call on a contract from an address other than the new version

**Representation:** `("NOT_MIGRATION_TARGET", ())`.

<a name="errors-NOT_OWNER"></a>

---

### `NOT_OWNER`

**Class:** Action exception

**Fires if:** The sender of transaction is not owner

**Representation:** `("NOT_OWNER", ())`.

<a name="errors-NOT_PENDING_ADMIN"></a>

---

### `NOT_PENDING_ADMIN`

**Class:** Action exception

**Fires if:** Received an `accept_ownership` from an address other than what is in the pending owner field

**Representation:** `("NOT_PENDING_ADMIN", ())`.

<a name="errors-OUT_OF_BOUND_QUORUM_THRESHOLD"></a>

---

### `OUT_OF_BOUND_QUORUM_THRESHOLD`

**Class:** Action exception

**Fires if:** Trying to set quorum threshold that is out of bound

**Representation:** `("OUT_OF_BOUND_QUORUM_THRESHOLD", ())`.

<a name="errors-OUT_OF_BOUND_VOTING_PERIOD"></a>

---

### `OUT_OF_BOUND_VOTING_PERIOD`

**Class:** Action exception

**Fires if:** Trying to set voting period that is out of bound.

**Representation:** `("OUT_OF_BOUND_VOTING_PERIOD", ())`.

<a name="errors-PROPOSAL_INSUFFICIENT_BALANCE"></a>

---

### `PROPOSAL_INSUFFICIENT_BALANCE`

**Class:** Action exception

**Fires if:** Trying to propose a proposal without having enough unfrozen token

**Representation:** `("PROPOSAL_INSUFFICIENT_BALANCE", ())`.

<a name="errors-PROPOSAL_NOT_EXIST"></a>

---

### `PROPOSAL_NOT_EXIST`

**Class:** Action exception

**Fires if:** Trying to vote on a proposal that does not exist

**Representation:** `("PROPOSAL_NOT_EXIST", ())`.

<a name="errors-PROPOSAL_NOT_UNIQUE"></a>

---

### `PROPOSAL_NOT_UNIQUE`

**Class:** Action exception

**Fires if:** Trying to propose a proposal that is already existed in the Storage.

**Representation:** `("PROPOSAL_NOT_UNIQUE", ())`.

<a name="errors-PROPOSER_NOT_EXIST_IN_LEDGER"></a>

---

### `PROPOSER_NOT_EXIST_IN_LEDGER`

**Class:** Action exception

**Fires if:** Expect a proposer address to exist in Ledger but it is not found (Impossible Case)

**Representation:** `("PROPOSER_NOT_EXIST_IN_LEDGER", ())`.

<a name="errors-VOTING_INSUFFICIENT_BALANCE"></a>

---

### `VOTING_INSUFFICIENT_BALANCE`

**Class:** Action exception

**Fires if:** Trying to vote on a proposal without having enough unfrozen token

**Representation:** `("VOTING_INSUFFICIENT_BALANCE", ())`.

<a name="errors-VOTING_PERIOD_OVER"></a>

---

### `VOTING_PERIOD_OVER`

**Class:** Action exception

**Fires if:** Trying to vote on a proposal that is already ended

**Representation:** `("VOTING_PERIOD_OVER", ())`.
