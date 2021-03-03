# Registry DAO

**Code revision:** [bfd18d7](https://github.com/tqtezos/baseDAO/tree/bfd18d7790842846dcd1790f462ffa31d38a2ca1) *(Wed Mar 3 15:09:58 2021 -0300)*



Registry DAO is a decentralized key-value storage of arbitrary data.
It's parameterized by two types: `k` (for key) and `v` (for value).
In UI, these `k` and `v` parameters both will be `bytes` and will be converted to application-level types externally.
Application-level types can be stored as part of the contract's metadata as specified in TZIP-16.


It contains standard FA2 entrypoints, plus some extra ones including proposal and
migration entrypoints. It supports two types of token_id - frozen (token_id = 1) and unfrozen (token_id = 0).


<a name="section-Table-of-contents"></a>

## Table of contents

- [Haskell ⇄ Michelson conversion](#section-Haskell-c8644-Michelson-conversion)
- [Storage](#section-Storage)
  - [Storage](#storage-Storage)
- [Entrypoints](#section-Entrypoints)
  - [accept_ownership](#entrypoints-accept_ownership)
  - [burn](#entrypoints-burn)
  - [call_FA2](#entrypoints-call_FA2)
  - [callCustom](#entrypoints-callCustom)
  - [confirm_migration](#entrypoints-confirm_migration)
  - [drop_proposal](#entrypoints-drop_proposal)
  - [flush](#entrypoints-flush)
  - [getVotePermitCounter](#entrypoints-getVotePermitCounter)
  - [migrate](#entrypoints-migrate)
  - [mint](#entrypoints-mint)
  - [propose](#entrypoints-propose)
  - [set_quorum_threshold](#entrypoints-set_quorum_threshold)
  - [set_voting_period](#entrypoints-set_voting_period)
  - [transfer_contract_tokens](#entrypoints-transfer_contract_tokens)
  - [transfer_ownership](#entrypoints-transfer_ownership)
  - [vote](#entrypoints-vote)
  - [get_total_supply](#entrypoints-get_total_supply)

**[Definitions](#definitions)**

- [Types](#section-Types)
  - [()](#types-lparenrparen)
  - [(a, b)](#types-lparenacomma-brparen)
  - [Address](#types-Address)
  - [AgoraPostId](#types-AgoraPostId)
  - [BalanceRequestItem](#types-BalanceRequestItem)
  - [BalanceResponseItem](#types-BalanceResponseItem)
  - [BigMap](#types-BigMap)
  - [Bool](#types-Bool)
  - [BurnParam](#types-BurnParam)
  - [ByteString](#types-ByteString)
  - [ChainId](#types-ChainId)
  - [ConfigProposal](#types-ConfigProposal)
  - [Contract](#types-Contract)
  - [DataToSign](#types-DataToSign)
  - [Empty](#types-Empty)
  - [Hash](#types-Hash)
  - [Integer](#types-Integer)
  - [List](#types-List)
  - [Map](#types-Map)
  - [Maybe](#types-Maybe)
  - [MigrationStatus](#types-MigrationStatus)
  - [MintParam](#types-MintParam)
  - [Named entry](#types-Named-entry)
  - [Natural](#types-Natural)
  - [Nonce](#types-Nonce)
  - [NormalProposal](#types-NormalProposal)
  - [OperatorParam](#types-OperatorParam)
  - [Packed](#types-Packed)
  - [Parameter](#types-Parameter)
  - [Permit](#types-Permit)
  - [PermitProtected](#types-PermitProtected)
  - [Proposal](#types-Proposal)
  - [ProposeParams](#types-ProposeParams)
  - [PublicKey](#types-PublicKey)
  - [RegistryDaoContractExtra](#types-RegistryDaoContractExtra)
  - [RegistryDaoProposalMetadata](#types-RegistryDaoProposalMetadata)
  - [RegistryEntry](#types-RegistryEntry)
  - [RegistryUpdate](#types-RegistryUpdate)
  - [Set](#types-Set)
  - [Signature](#types-Signature)
  - [SomeType](#types-SomeType)
  - [TSignature](#types-TSignature)
  - [Text](#types-Text)
  - [Timestamp](#types-Timestamp)
  - [TokenId](#types-TokenId)
  - [TransferContractTokensParam](#types-TransferContractTokensParam)
  - [TransferDestination](#types-TransferDestination)
  - [TransferItem](#types-TransferItem)
  - [UpdateOperator](#types-UpdateOperator)
  - [UpdateReceivers](#types-UpdateReceivers)
  - [View](#types-View)
  - [VoteParam](#types-VoteParam)
- [Errors](#section-Errors)
  - [BAD_ENTRYPOINT_PARAMETER](#errors-BAD_ENTRYPOINT_PARAMETER)
  - [FA2_INSUFFICIENT_BALANCE](#errors-FA2_INSUFFICIENT_BALANCE)
  - [FA2_NOT_OPERATOR](#errors-FA2_NOT_OPERATOR)
  - [FA2_TOKEN_UNDEFINED](#errors-FA2_TOKEN_UNDEFINED)
  - [FAIL_DROP_PROPOSAL_NOT_ACCEPTED](#errors-FAIL_DROP_PROPOSAL_NOT_ACCEPTED)
  - [FAIL_DROP_PROPOSAL_NOT_OVER](#errors-FAIL_DROP_PROPOSAL_NOT_OVER)
  - [FAIL_PROPOSAL_CHECK](#errors-FAIL_PROPOSAL_CHECK)
  - [FAIL_TRANSFER_CONTRACT_TOKENS](#errors-FAIL_TRANSFER_CONTRACT_TOKENS)
  - [FROZEN_TOKEN_NOT_TRANSFERABLE](#errors-FROZEN_TOKEN_NOT_TRANSFERABLE)
  - [InternalError](#errors-InternalError)
  - [MAX_PROPOSALS_REACHED](#errors-MAX_PROPOSALS_REACHED)
  - [MAX_VOTES_REACHED](#errors-MAX_VOTES_REACHED)
  - [MIGRATED](#errors-MIGRATED)
  - [MISSIGNED](#errors-MISSIGNED)
  - [NEGATIVE_TOTAL_SUPPLY](#errors-NEGATIVE_TOTAL_SUPPLY)
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
- [Referenced hash algorithms](#section-Referenced-hash-algorithms)



<a name="section-Haskell-c8644-Michelson-conversion"></a>

## Haskell ⇄ Michelson conversion

This smart contract is developed in Haskell using the [Morley framework](https://gitlab.com/morley-framework/morley). Documentation mentions Haskell types that can be used for interaction with this contract from Haskell, but for each Haskell type we also mention its Michelson representation to make interactions outside of Haskell possible.

There are multiple ways to interact with this contract:

* Use this contract in your Haskell application, thus all operation submissions should be handled separately, e.g. via calling `tezos-client`, which will communicate with the `tezos-node`. In order to be able to call `tezos-client` you'll need to be able to construct Michelson values from Haskell.

  The easiest way to do that is to serialize Haskell value using `lPackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs) module, encode resulting bytestring to hexadecimal representation using `encodeHex` function. Resulting hexadecimal encoded bytes sequence can be decoded back to Michelson value via `tezos-client unpack michelson data`.

  Reverse conversion from Michelson value to the Haskell value can be done by serializing Michelson value using `tezos-client hash data` command, resulting `Raw packed data` should be decoded from the hexadecimal representation using `decodeHex` and deserialized to the Haskell value via `lUnpackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs).

* Construct values for this contract directly on Michelson level using types provided in the documentation.

<a name="section-Storage"></a>

## Storage

<a name="storage-Storage"></a>

---

### `Storage`

Storage type for baseDAO contract

**Structure (example):** `Storage Natural MText` = 
  * ***sLedger*** :[`BigMap`](#types-BigMap) ([`Address`](#types-Address), [`TokenId`](#types-TokenId)) [`Natural`](#types-Natural)
  * ***sOperators*** :[`BigMap`](#types-BigMap) (***owner*** : [`Address`](#types-Address), ***operator*** : [`Address`](#types-Address)) [`()`](#types-lparenrparen)
  * ***sTokenAddress*** :[`Address`](#types-Address)
  * ***sAdmin*** :[`Address`](#types-Address)
  * ***sPendingOwner*** :[`Address`](#types-Address)
  * ***sMigrationStatus*** :[`MigrationStatus`](#types-MigrationStatus)
  * ***sVotingPeriod*** :[`Natural`](#types-Natural)
  * ***sQuorumThreshold*** :[`Natural`](#types-Natural)
  * ***sExtra*** :[`Natural`](#types-Natural)
  * ***sProposals*** :[`BigMap`](#types-BigMap) ([`Hash`](#types-Hash) [`Blake2b`](#hash-alg-Blake2b) ([`Packed`](#types-Packed) ([`ProposeParams`](#types-ProposeParams) [`Text`](#types-Text), [`Address`](#types-Address)))) ([`Proposal`](#types-Proposal) [`Text`](#types-Text))
  * ***sProposalKeyListSortByDate*** :[`Set`](#types-Set) ([`Timestamp`](#types-Timestamp), [`Hash`](#types-Hash) [`Blake2b`](#hash-alg-Blake2b) ([`Packed`](#types-Packed) ([`ProposeParams`](#types-ProposeParams) [`Text`](#types-Text), [`Address`](#types-Address))))
  * ***sPermitsCounter*** :[`Nonce`](#types-Nonce)
  * ***sMetadata*** :[`BigMap`](#types-BigMap) [`Text`](#types-Text) [`ByteString`](#types-ByteString)
  * ***sTotalSupply*** :[`Map`](#types-Map) [`TokenId`](#types-TokenId) [`Natural`](#types-Natural)

**Final Michelson representation (example):** `Storage Natural MText` = `pair (pair (pair (big_map (pair address nat) nat) (pair (big_map (pair address address) unit) address)) (pair (pair address address) (pair (or unit (or address address)) nat))) (pair (pair nat (pair nat (big_map bytes (pair (pair nat (pair nat timestamp)) (pair (pair string address) (pair nat (list (pair address nat)))))))) (pair (pair (set (pair timestamp bytes)) nat) (pair (big_map string bytes) (map nat nat))))`



<a name="section-Entrypoints"></a>

## Entrypoints

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

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NEGATIVE_TOTAL_SUPPLY`](#errors-NEGATIVE_TOTAL_SUPPLY) — An error occured when trying to burn an amount of token more than its current total supply



<a name="entrypoints-call_FA2"></a>

---

### `call_FA2`

Entrypoint to be called if you want to use one of FA2 entrypoints.


**Argument:** 
  + **In Haskell:** [`Parameter`](#types-Parameter)
  + **In Michelson:** `(or (or (pair (list %requests (pair (address %owner) (nat %token_id))) (contract %callback (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance))))) (list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount))))))) (list (or (pair %add_operator (address %owner) (pair (address %operator) (nat %token_id))) (pair %remove_operator (address %owner) (pair (address %operator) (nat %token_id))))))`
    + **Example:** <span id="example-id">`Left (Left (Pair { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0 } "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"))`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `call_FA2` entrypoint passing the constructed argument.
</details>
<p>



<a name="section-FA2-entrypoints"></a>

#### FA2 entrypoints

<a name="entrypoints-balance_of"></a>

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



<a name="entrypoints-transfer"></a>

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

* [`NEGATIVE_TOTAL_SUPPLY`](#errors-NEGATIVE_TOTAL_SUPPLY) — An error occured when trying to burn an amount of token more than its current total supply



<a name="entrypoints-update_operators"></a>

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





<a name="entrypoints-callCustom"></a>

---

### `callCustom`

Additional entrypoints specific to the given specific DAO.


**Argument:** 
  + **In Haskell:** [`Empty`](#types-Empty)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `callCustom` entrypoint passing the constructed argument.
</details>
<p>





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



<a name="entrypoints-drop_proposal"></a>

---

### `drop_proposal`

Delete a finished and accepted proposal that is not flushed. Tokens frozen for this
proposal are returned to the proposer and voters in full. The decision lambda is skipped.

This entrypoint should only be used when there is a proposal that is stuck due to having a
failing decision lambda.


**Argument:** 
  + **In Haskell:** [`Hash`](#types-Hash) [`Blake2b`](#hash-alg-Blake2b) ([`Packed`](#types-Packed) ([`ProposeParams`](#types-ProposeParams) ([`RegistryDaoProposalMetadata`](#types-RegistryDaoProposalMetadata) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString)), [`Address`](#types-Address)))
  + **In Michelson:** `bytes`
    + **Example:** <span id="example-id">`0x0a`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `drop_proposal` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`PROPOSAL_NOT_EXIST`](#errors-PROPOSAL_NOT_EXIST) — Trying to vote on a proposal that does not exist

* [`PROPOSER_NOT_EXIST_IN_LEDGER`](#errors-PROPOSER_NOT_EXIST_IN_LEDGER) — Expect a proposer address to exist in Ledger but it is not found (Impossible Case)

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NEGATIVE_TOTAL_SUPPLY`](#errors-NEGATIVE_TOTAL_SUPPLY) — An error occured when trying to burn an amount of token more than its current total supply

* [`FAIL_DROP_PROPOSAL_NOT_ACCEPTED`](#errors-FAIL_DROP_PROPOSAL_NOT_ACCEPTED) — An error occurred when trying to drop a proposal due to the proposal is not an accepted proposal

* [`FAIL_DROP_PROPOSAL_NOT_OVER`](#errors-FAIL_DROP_PROPOSAL_NOT_OVER) — An error occurred when trying to drop a proposal due to the proposal's voting period is not over



<a name="entrypoints-flush"></a>

---

### `flush`

Finish voting process on all proposals where the voting period is over.
Returns an amount to the proposer, determined by the result of voting.
There is a possibility of some tokens being lost due to administrator perform
of burn or transfer operation.
If the proposal is accepted, the decision lambda is called.


**Argument:** 
  + **In Haskell:** [`Natural`](#types-Natural)
  + **In Michelson:** `nat`
    + **Example:** <span id="example-id">`0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `flush` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`BAD_ENTRYPOINT_PARAMETER`](#errors-BAD_ENTRYPOINT_PARAMETER) — Value passed to the entrypoint is not valid

* [`PROPOSAL_NOT_EXIST`](#errors-PROPOSAL_NOT_EXIST) — Trying to vote on a proposal that does not exist

* [`PROPOSER_NOT_EXIST_IN_LEDGER`](#errors-PROPOSER_NOT_EXIST_IN_LEDGER) — Expect a proposer address to exist in Ledger but it is not found (Impossible Case)

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NEGATIVE_TOTAL_SUPPLY`](#errors-NEGATIVE_TOTAL_SUPPLY) — An error occured when trying to burn an amount of token more than its current total supply



<a name="entrypoints-getVotePermitCounter"></a>

---

### `getVotePermitCounter`

Returns the next nonce value with which a permit should be created.

Return value increases by number of votes where a permit was provided
with each successful call of an entrypoint.


**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Nonce`](#types-Nonce)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getVotePermitCounter` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-migrate"></a>

---

### `migrate`

Asks an address to migrate the contract to it.
The contract is not considered migrated, until it receives confirm_migration call.


**Argument:** 
  + **In Haskell:** ***newAddress*** : [`Address`](#types-Address)
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

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id



<a name="entrypoints-propose"></a>

---

### `propose`

Saves the proposal with specific id and freezes the amount of sender tokens equal to the given amount.
The sender must have enough unfrozen tokens.
The sender's amount of frozen tokens is increased by proposal ammount. And the amount of unfrozen one
is decreased by the same value.


**Argument:** 
  + **In Haskell:** [`ProposeParams`](#types-ProposeParams) ([`RegistryDaoProposalMetadata`](#types-RegistryDaoProposalMetadata) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString))
  + **In Michelson:** `(pair (nat %frozen_token) (or %proposal_metadata (pair %proposal_type (nat %agora_post_id) (list %diff (pair (bytes %key) (option %new_value bytes)))) (or (pair %proposal_type (pair (option %frozen_scale_value nat) (option %frozen_extra_value nat)) (pair (option %slash_scale_value nat) (pair (option %slash_division_value nat) (option %max_proposal_size nat)))) (or %receivers_type (list %receivers address) (list %receivers address)))))`
    + **Example:** <span id="example-id">`Pair 0 (Left (Pair 0 { Pair 0x0a (Some 0x0a) }))`</span>

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

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NEGATIVE_TOTAL_SUPPLY`](#errors-NEGATIVE_TOTAL_SUPPLY) — An error occured when trying to burn an amount of token more than its current total supply

* [`PROPOSAL_NOT_UNIQUE`](#errors-PROPOSAL_NOT_UNIQUE) — Trying to propose a proposal that is already existed in the Storage.



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



<a name="entrypoints-transfer_ownership"></a>

---

### `transfer_ownership`

Asks an Address to become an admin. Can be called only by current administrator. The admin duties transfer only when the
requested address accepts ownership. If called multiple times, only the last called address can accept ownership.


**Argument:** 
  + **In Haskell:** ***newOwner*** : [`Address`](#types-Address)
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



<a name="entrypoints-vote"></a>

---

### `vote`

For each vote params in a given list vote for the proposal with that id. Thus the sender can vote many proposals
(or one proposal multiple times) in a single call.
The sender must have an amount required for all votings.


**Argument:** 
  + **In Haskell:** [`List`](#types-List) ([`PermitProtected`](#types-PermitProtected) ([`VoteParam`](#types-VoteParam) ([`RegistryDaoProposalMetadata`](#types-RegistryDaoProposalMetadata) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString))))
  + **In Michelson:** `(list (pair :permit_protected (pair (bytes %proposal_key) (pair (bool %vote_type) (nat %vote_amount))) (option %permit (pair (key %key) (signature %signature)))))`
    + **Example:** <span id="example-id">`{ Pair (Pair 0x0a (Pair True 0)) (Some (Pair "edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V" "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb")) }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `vote` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`MISSIGNED`](#errors-MISSIGNED) — Invalid signature provided.

* [`PROPOSAL_NOT_EXIST`](#errors-PROPOSAL_NOT_EXIST) — Trying to vote on a proposal that does not exist

* [`MAX_VOTES_REACHED`](#errors-MAX_VOTES_REACHED) — Trying to vote on a proposal when the votes max amount of that proposal is already reached

* [`VOTING_PERIOD_OVER`](#errors-VOTING_PERIOD_OVER) — Trying to vote on a proposal that is already ended

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens

* [`VOTING_INSUFFICIENT_BALANCE`](#errors-VOTING_INSUFFICIENT_BALANCE) — Trying to vote on a proposal without having enough unfrozen token

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NEGATIVE_TOTAL_SUPPLY`](#errors-NEGATIVE_TOTAL_SUPPLY) — An error occured when trying to burn an amount of token more than its current total supply



<a name="entrypoints-get_total_supply"></a>

---

### `get_total_supply`

Return the total number of tokens for the given token-id if known or fail if not.


**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`TokenId`](#types-TokenId) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (nat %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair 0 "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `get_total_supply` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id







# Definitions

<a name="section-Types"></a>

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



<a name="types-Address"></a>

---

### `Address`

Address primitive.

Unlike Michelson's `address`, it is assumed not to contain an entrypoint name,
even if it refers to a contract; this won't be checked, so passing an entrypoint
name may result in unexpected errors.


**Final Michelson representation:** `address`



<a name="types-AgoraPostId"></a>

---

### `AgoraPostId`

Describe an Agora post ID.

**Structure:** [`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-BalanceRequestItem"></a>

---

### `BalanceRequestItem`

Describes a request for an owner's balance

**Structure:** 
  * ***owner*** :[`Address`](#types-Address)
  * ***tokenId*** :[`TokenId`](#types-TokenId)

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
  * ***from_*** :[`Address`](#types-Address)
  * ***tokenId*** :[`TokenId`](#types-TokenId)
  * ***amount*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address (pair nat nat)`



<a name="types-ByteString"></a>

---

### `ByteString`

Bytes primitive.

**Final Michelson representation:** `bytes`



<a name="types-ChainId"></a>

---

### `ChainId`

Identifier of the current chain.

**Final Michelson representation:** `chain_id`



<a name="types-ConfigProposal"></a>

---

### `ConfigProposal`

Describe a configuration proposal in Registry DAO. It is used to update the configuration values in contract extra that affect the process of checking if a proposal is valid or not and how tokens are slash when a proposal is rejected.

**Structure:** 
  * ***frozenScaleValue*** :[`Maybe`](#types-Maybe) [`Natural`](#types-Natural)
  * ***frozenExtraValue*** :[`Maybe`](#types-Maybe) [`Natural`](#types-Natural)
  * ***slashScaleValue*** :[`Maybe`](#types-Maybe) [`Natural`](#types-Natural)
  * ***slashDivisionValue*** :[`Maybe`](#types-Maybe) [`Natural`](#types-Natural)
  * ***maxProposalSize*** :[`Maybe`](#types-Maybe) [`Natural`](#types-Natural)

**Final Michelson representation:** `pair (pair (option nat) (option nat)) (pair (option nat) (pair (option nat) (option nat)))`



<a name="types-Contract"></a>

---

### `Contract`

Contract primitive with given type of parameter.

**Final Michelson representation (example):** `ContractRef Integer` = `contract int`



<a name="types-DataToSign"></a>

---

### `DataToSign`

A wrapper over data that is to be signed.

Aside from the original data, this contains elements that ensure the result
to be globally unique in order to avoid replay attacks:
* Chain id
* Address of the contract
* Nonce - suitable nonce can be fetched using the dedicated endpoint.


**Structure (example):** `DataToSign MText` = 
  * ***dsChainId*** :[`ChainId`](#types-ChainId)
  * ***dsContract*** :[`Address`](#types-Address)
  * ***dsNonce*** :[`Nonce`](#types-Nonce)
  * ***dsData*** :[`Text`](#types-Text)

**Final Michelson representation (example):** `DataToSign MText` = `pair (pair chain_id address) (pair nat string)`



<a name="types-Empty"></a>

---

### `Empty`

Type which should never be constructed.

If appears as part of entrypoint argument, this means that the entrypoint should never be called.

**Structure:** [`()`](#types-lparenrparen)

**Final Michelson representation:** `unit`



<a name="types-Hash"></a>

---

### `Hash`

Hash of a value.

First type argument denotes algorithm used to compute the hash, and the second
argument describes the data being hashed.


**Structure (example):** `Hash Blake2b ByteString` = [`ByteString`](#types-ByteString)

**Final Michelson representation (example):** `Hash Blake2b ByteString` = `bytes`



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



<a name="types-Map"></a>

---

### `Map`

Map primitive.

**Final Michelson representation (example):** `Map Integer Natural` = `map int nat`



<a name="types-Maybe"></a>

---

### `Maybe`

Option primitive.

**Final Michelson representation (example):** `Maybe Integer` = `option int`



<a name="types-MigrationStatus"></a>

---

### `MigrationStatus`

Migration status of the contract

**Structure:** *one of* 
+ **NotInMigration**()
+ **MigratingTo**[`Address`](#types-Address)
+ **MigratedTo**[`Address`](#types-Address)


**Final Michelson representation:** `or unit (or address address)`



<a name="types-MintParam"></a>

---

### `MintParam`

Describes whose account, which token id and in what amount to mint

**Structure:** 
  * ***to_*** :[`Address`](#types-Address)
  * ***tokenId*** :[`TokenId`](#types-TokenId)
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



<a name="types-Nonce"></a>

---

### `Nonce`

Contract-local nonce used to make some data unique.

**Structure:** [`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-NormalProposal"></a>

---

### `NormalProposal`

Describe a proposal for add/update/delete an item in the registry.

**Structure (example):** `NormalProposal ByteString ByteString` = 
  * ***npAgoraPostId*** :[`AgoraPostId`](#types-AgoraPostId)
  * ***npDiff*** :[`List`](#types-List) ([`RegistryUpdate`](#types-RegistryUpdate) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString))

**Final Michelson representation (example):** `NormalProposal ByteString ByteString` = `pair nat (list (pair bytes (option bytes)))`



<a name="types-OperatorParam"></a>

---

### `OperatorParam`

Describes an address authorized to transfer tokens on behalf of a token owner

**Structure:** 
  * ***owner*** :[`Address`](#types-Address)
  * ***operator*** :[`Address`](#types-Address)
  * ***tokenId*** :[`TokenId`](#types-TokenId)

**Final Michelson representation:** `pair address (pair address nat)`



<a name="types-Packed"></a>

---

### `Packed`

Packed value of the given type.
This exactly matches the result of Michelson `PACK` instruction application
to the given value.


**Structure (example):** `Packed (MText,Integer)` = [`ByteString`](#types-ByteString)

**Final Michelson representation (example):** `Packed (MText,Integer)` = `bytes`



<a name="types-Parameter"></a>

---

### `Parameter`

Describes the FA2 operations.

**Structure:** *one of* 
+ **Balance_of**([`View`](#types-View) ([`List`](#types-List) [`BalanceRequestItem`](#types-BalanceRequestItem)) ([`List`](#types-List) [`BalanceResponseItem`](#types-BalanceResponseItem)))
+ **Transfer**([`List`](#types-List) [`TransferItem`](#types-TransferItem))
+ **Update_operators**([`List`](#types-List) [`UpdateOperator`](#types-UpdateOperator))


**Final Michelson representation:** `or (or (pair (list (pair address nat)) (contract (list (pair (pair address nat) nat)))) (list (pair address (list (pair address (pair nat nat)))))) (list (or (pair address (pair address nat)) (pair address (pair address nat))))`



<a name="types-Permit"></a>

---

### `Permit`

Permission for executing an action from another user's behalf.

This contains public key of that user and signed argument for the entrypoint.
Type parameter of `Permit` stands for the entrypoint argument type.


**Structure (example):** `Permit Integer` = 
  * ***pKey*** :[`PublicKey`](#types-PublicKey)
  * ***pSignature*** :[`TSignature`](#types-TSignature) ([`Packed`](#types-Packed) ([`DataToSign`](#types-DataToSign) [`Integer`](#types-Integer)))

**Final Michelson representation (example):** `Permit Integer` = `pair key signature`



<a name="types-PermitProtected"></a>

---

### `PermitProtected`

Marks an entrypoint with given argument type as callable from another
user's behalf.

* If `permit` part is present, we use the supplied information to identify
the original author of the request and validate that it is constructed by
them.
* If `permit` part is absent, we assume that entrypoint is called from the
current sender's behalf.


**Structure (example):** `PermitProtected Integer` = 
  * ***ppArgument*** :[`Integer`](#types-Integer)
  * ***ppPermit*** :[`Maybe`](#types-Maybe) ([`Permit`](#types-Permit) [`Integer`](#types-Integer))

**Final Michelson representation (example):** `PermitProtected Integer` = `pair int (option (pair key signature))`



<a name="types-Proposal"></a>

---

### `Proposal`

Contract's storage holding a big_map with all balances and the operators.

**Structure (example):** `Proposal ()` = 
  * ***pUpvotes*** :[`Natural`](#types-Natural)
  * ***pDownvotes*** :[`Natural`](#types-Natural)
  * ***pStartDate*** :[`Timestamp`](#types-Timestamp)
  * ***pMetadata*** :[`()`](#types-lparenrparen)
  * ***pProposer*** :[`Address`](#types-Address)
  * ***pProposerFrozenToken*** :[`Natural`](#types-Natural)
  * ***pVoters*** :[`List`](#types-List) ([`Address`](#types-Address), [`Natural`](#types-Natural))

**Final Michelson representation (example):** `Proposal ()` = `pair (pair nat (pair nat timestamp)) (pair (pair unit address) (pair nat (list (pair address nat))))`



<a name="types-ProposeParams"></a>

---

### `ProposeParams`

Describes the how many proposer's frozen tokens will be frozen and the proposal metadata

**Structure (example):** `ProposeParams ()` = 
  * ***ppFrozenToken*** :[`Natural`](#types-Natural)
  * ***ppProposalMetadata*** :[`()`](#types-lparenrparen)

**Final Michelson representation (example):** `ProposeParams ()` = `pair nat unit`



<a name="types-PublicKey"></a>

---

### `PublicKey`

PublicKey primitive.

**Final Michelson representation:** `key`



<a name="types-RegistryDaoContractExtra"></a>

---

### `RegistryDaoContractExtra`

Describe the contract extra fields of a registry DAO. It contain a registry as a `BigMap` of a key `k` and a value of `RegistryEntry v`. It also contains various configurable values that can  be updated via `ConfigProposal`

**Structure (example):** `RegistryDaoContractExtra ByteString ByteString` = 
  * ***ceRegistry*** :[`BigMap`](#types-BigMap) [`ByteString`](#types-ByteString) ([`RegistryEntry`](#types-RegistryEntry) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString))
  * ***ceProposalReceivers*** :[`Set`](#types-Set) [`Address`](#types-Address)
  * ***ceFrozenScaleValue*** :[`Natural`](#types-Natural)
  * ***ceFrozenExtraValue*** :[`Natural`](#types-Natural)
  * ***ceSlashScaleValue*** :[`Natural`](#types-Natural)
  * ***ceSlashDivisionValue*** :[`Natural`](#types-Natural)
  * ***ceMaxProposalSize*** :[`Natural`](#types-Natural)

**Final Michelson representation (example):** `RegistryDaoContractExtra ByteString ByteString` = `pair (pair (big_map bytes (pair (option bytes) (pair bytes timestamp))) (pair (set address) nat)) (pair (pair nat nat) (pair nat nat))`



<a name="types-RegistryDaoProposalMetadata"></a>

---

### `RegistryDaoProposalMetadata`

Describe the metadata of a proposal in Registry DAO. In Registry DAO, there are 2 types of proposals: a registry proposal, represented as `NormalProposal k v` and a configuration proposal represented as `ConfigProposal`.

**Structure (example):** `RegistryDaoProposalMetadata ByteString ByteString` = *one of* 
+ **NormalProposalType**([`NormalProposal`](#types-NormalProposal) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString))
+ **ConfigProposalType**[`ConfigProposal`](#types-ConfigProposal)
+ **UpdateReceiversType**[`UpdateReceivers`](#types-UpdateReceivers)


**Final Michelson representation (example):** `RegistryDaoProposalMetadata ByteString ByteString` = `or (pair nat (list (pair bytes (option bytes)))) (or (pair (pair (option nat) (option nat)) (pair (option nat) (pair (option nat) (option nat)))) (or (list address) (list address)))`



<a name="types-RegistryEntry"></a>

---

### `RegistryEntry`

Describe the value in registry map. It represents the actual item of the registry as `Maybe v`. `None` represents the deletion of item and `Some v` represents the existence of an item.It also contains the last proposal's agora post id that affects this item and its last updated time.

**Structure (example):** `RegistryEntry ByteString ByteString` = 
  * ***reValue*** :[`Maybe`](#types-Maybe) [`ByteString`](#types-ByteString)
  * ***reAffectedProposalKey*** :[`Hash`](#types-Hash) [`Blake2b`](#hash-alg-Blake2b) ([`Packed`](#types-Packed) ([`ProposeParams`](#types-ProposeParams) ([`RegistryDaoProposalMetadata`](#types-RegistryDaoProposalMetadata) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString)), [`Address`](#types-Address)))
  * ***reLastUpdated*** :[`Timestamp`](#types-Timestamp)

**Final Michelson representation (example):** `RegistryEntry ByteString ByteString` = `pair (option bytes) (pair bytes timestamp)`



<a name="types-RegistryUpdate"></a>

---

### `RegistryUpdate`

Describe a proposed change to an items in the registry.

**Structure (example):** `RegistryUpdate ByteString ByteString` = 
  * ***ruKey*** :[`ByteString`](#types-ByteString)
  * ***ruNewValue*** :[`Maybe`](#types-Maybe) [`ByteString`](#types-ByteString)

**Final Michelson representation (example):** `RegistryUpdate ByteString ByteString` = `pair bytes (option bytes)`



<a name="types-Set"></a>

---

### `Set`

Set primitive.

**Final Michelson representation (example):** `Set Integer` = `set int`



<a name="types-Signature"></a>

---

### `Signature`

Signature primitive.

**Final Michelson representation:** `signature`



<a name="types-SomeType"></a>

---

### `SomeType`

Some type, may differ in various situations.

**Final Michelson representation (example):** `SomeType` = `unit`



<a name="types-TSignature"></a>

---

### `TSignature`

Signature for data of the given type.

**Structure (example):** `TSignature (MText,Integer)` = [`Signature`](#types-Signature)

**Final Michelson representation (example):** `TSignature (MText,Integer)` = `signature`



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



<a name="types-TokenId"></a>

---

### `TokenId`

Token identifier as defined by [TZIP-12](https://gitlab.com/tzip/tzip/-/blob/1f83a3671cdff3ab4517bfa9ee5a57fc5276d4ff/proposals/tzip-12/tzip-12.md#general).

**Structure:** [`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-TransferContractTokensParam"></a>

---

### `TransferContractTokensParam`

TODO

**Structure:** 
  * ***contractAddress*** :[`Address`](#types-Address)
  * ***params*** :[`List`](#types-List) [`TransferItem`](#types-TransferItem)

**Final Michelson representation:** `pair address (list (pair address (list (pair address (pair nat nat)))))`



<a name="types-TransferDestination"></a>

---

### `TransferDestination`

Describes the amount of tokens to transfer and to whom

**Structure:** 
  * ***to*** :[`Address`](#types-Address)
  * ***tokenId*** :[`TokenId`](#types-TokenId)
  * ***amount*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair address (pair nat nat)`



<a name="types-TransferItem"></a>

---

### `TransferItem`

Describes a transfer operation

**Structure:** 
  * ***from*** :[`Address`](#types-Address)
  * ***txs*** :[`List`](#types-List) [`TransferDestination`](#types-TransferDestination)

**Final Michelson representation:** `pair address (list (pair address (pair nat nat)))`



<a name="types-UpdateOperator"></a>

---

### `UpdateOperator`

Describes the operator update operation.

**Structure:** *one of* 
+ **AddOperator**[`OperatorParam`](#types-OperatorParam)
+ **RemoveOperator**[`OperatorParam`](#types-OperatorParam)


**Final Michelson representation:** `or (pair address (pair address nat)) (pair address (pair address nat))`



<a name="types-UpdateReceivers"></a>

---

### `UpdateReceivers`

Describe a proposal in Registry DAO. It is used to update the list of receiver proposal contract addresses stored in contract extra

**Structure:** *one of* 
+ **AddReceivers**([`List`](#types-List) [`Address`](#types-Address))
+ **RemoveReceivers**([`List`](#types-List) [`Address`](#types-Address))


**Final Michelson representation:** `or (list address) (list address)`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-4/tzip-4.md#view-entrypoints).

**Structure (example):** `View MText Integer` = 
  * [`Text`](#types-Text)
  * [`ContractRef`](#types-Contract) [`Integer`](#types-Integer)

**Final Michelson representation (example):** `View MText Integer` = `pair string (contract int)`



<a name="types-VoteParam"></a>

---

### `VoteParam`

Describes target proposal id, vote type and vote amount

**Structure (example):** `VoteParam MText` = 
  * ***vProposalKey*** :[`Hash`](#types-Hash) [`Blake2b`](#hash-alg-Blake2b) ([`Packed`](#types-Packed) ([`ProposeParams`](#types-ProposeParams) [`Text`](#types-Text), [`Address`](#types-Address)))
  * ***vVoteType*** :[`Bool`](#types-Bool)
  * ***vVoteAmount*** :[`Natural`](#types-Natural)

**Final Michelson representation (example):** `VoteParam MText` = `pair bytes (pair bool nat)`



<a name="section-Errors"></a>

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


<a name="errors-BAD_ENTRYPOINT_PARAMETER"></a>

---

### `BAD_ENTRYPOINT_PARAMETER`

**Class:** Bad argument

**Fires if:** Value passed to the entrypoint is not valid

**Representation:** `("BAD_ENTRYPOINT_PARAMETER", ())`.

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

<a name="errors-FAIL_DROP_PROPOSAL_NOT_ACCEPTED"></a>

---

### `FAIL_DROP_PROPOSAL_NOT_ACCEPTED`

**Class:** Action exception

**Fires if:** An error occurred when trying to drop a proposal due to the proposal is not an accepted proposal

**Representation:** `("FAIL_DROP_PROPOSAL_NOT_ACCEPTED", ())`.

<a name="errors-FAIL_DROP_PROPOSAL_NOT_OVER"></a>

---

### `FAIL_DROP_PROPOSAL_NOT_OVER`

**Class:** Action exception

**Fires if:** An error occurred when trying to drop a proposal due to the proposal's voting period is not over

**Representation:** `("FAIL_DROP_PROPOSAL_NOT_OVER", ())`.

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

Provided error argument will be of type [`Address`](#types-Address).

<a name="errors-MISSIGNED"></a>

---

### `MISSIGNED`

**Class:** Action exception

**Fires if:** Invalid signature provided.

**Representation:** `("MISSIGNED", <error argument>)`.

Provided error argument will be of type [`Packed`](#types-Packed) ([`DataToSign`](#types-DataToSign) [`SomeType`](#types-SomeType)).

<a name="errors-NEGATIVE_TOTAL_SUPPLY"></a>

---

### `NEGATIVE_TOTAL_SUPPLY`

**Class:** Action exception

**Fires if:** An error occured when trying to burn an amount of token more than its current total supply

**Representation:** `("NEGATIVE_TOTAL_SUPPLY", ())`.

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

<a name="section-Referenced-hash-algorithms"></a>

## Referenced hash algorithms

<a name="hash-alg-Blake2b"></a>

* Blake2b
