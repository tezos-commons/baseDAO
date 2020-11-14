# BaseDAO

"BaseDAO description"

## Entrypoints

<a name="entrypoints-call_FA2"></a>

---

### `call_FA2`

**Argument:** 
  + **In Haskell:** [`Parameter`](#types-Parameter)
  + **In Michelson:** `(or (or (list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))) (pair (list %requests (pair (address %owner) (nat %token_id))) (contract %callback (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))) (or (contract address) (list (or (pair %add_operator (address %owner) (pair (address %operator) (nat %token_id))) (pair %remove_operator (address %owner) (pair (address %operator) (nat %token_id)))))))`
    + **Example:** <span id="example-id">`Left (Left { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0) } })`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Call_FA2` constructor.
    + **In Haskell:** `Call_FA2 (·)`
    + **In Michelson:** `Left (Left (Left (·)))`
</details>
<p>



#### Entrypoints

<a name="entrypoints-transfer"></a>

---

##### `transfer`

**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`TransferItem`](#types-TransferItem)
  + **In Michelson:** `(list (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount))))))`
    + **Example:** <span id="example-id">`{ Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0) } }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Transfer` constructor.
    + **In Haskell:** `Transfer (·)`
    + **In Michelson:** `Left (Left (·))`
1. Wrap into `Call_FA2` constructor.
    + **In Haskell:** `Call_FA2 (·)`
    + **In Michelson:** `Left (Left (Left (·)))`
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`FA2_NOT_OPERATOR`](#errors-FA2_NOT_OPERATOR) — The sender of transfer is not the owner or the authorized operator

* [`FROZEN_TOKEN_NOT_TRANSFERABLE`](#errors-FROZEN_TOKEN_NOT_TRANSFERABLE) — The sender tries to transfer frozen token

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`FA2_INSUFFICIENT_BALANCE`](#errors-FA2_INSUFFICIENT_BALANCE) — The source of a transfer did not contain sufficient tokens



<a name="entrypoints-balance_of"></a>

---

##### `balance_of`

**Argument:** 
  + **In Haskell:** [`View`](#types-View) ([`List`](#types-List) [`BalanceRequestItem`](#types-BalanceRequestItem)) ([`List`](#types-List) [`BalanceResponseItem`](#types-BalanceResponseItem))
  + **In Michelson:** `(pair (list %requests (pair (address %owner) (nat %token_id))) (contract %callback (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))`
    + **Example:** <span id="example-id">`Pair { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0 } "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Balance_of` constructor.
    + **In Haskell:** `Balance_of (·)`
    + **In Michelson:** `Left (Right (·))`
1. Wrap into `Call_FA2` constructor.
    + **In Haskell:** `Call_FA2 (·)`
    + **In Michelson:** `Left (Left (Left (·)))`
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id



<a name="entrypoints-token_metadata_registry"></a>

---

##### `token_metadata_registry`

**Argument:** 
  + **In Haskell:** [`ContractRef`](#types-Contract) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(contract address)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Token_metadata_registry` constructor.
    + **In Haskell:** `Token_metadata_registry (·)`
    + **In Michelson:** `Right (Left (·))`
1. Wrap into `Call_FA2` constructor.
    + **In Haskell:** `Call_FA2 (·)`
    + **In Michelson:** `Left (Left (Left (·)))`
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract



<a name="entrypoints-update_operators"></a>

---

##### `update_operators`

**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`UpdateOperator`](#types-UpdateOperator)
  + **In Michelson:** `(list (or (pair %add_operator (address %owner) (pair (address %operator) (nat %token_id))) (pair %remove_operator (address %owner) (pair (address %operator) (nat %token_id)))))`
    + **Example:** <span id="example-id">`{ Left (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0)) }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Update_operators` constructor.
    + **In Haskell:** `Update_operators (·)`
    + **In Michelson:** `Right (Right (·))`
1. Wrap into `Call_FA2` constructor.
    + **In Haskell:** `Call_FA2 (·)`
    + **In Michelson:** `Left (Left (Left (·)))`
</details>
<p>



**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`FA2_TOKEN_UNDEFINED`](#errors-FA2_TOKEN_UNDEFINED) — Contract received an unsupported token id

* [`NOT_OWNER`](#errors-NOT_OWNER) — The sender of transaction is not owner





<a name="entrypoints-transfer_ownership"></a>

---

### `transfer_ownership`

**Argument:** 
  + **In Haskell:** ***newOwner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(address :newOwner)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Transfer_ownership` constructor.
    + **In Haskell:** `Transfer_ownership (·)`
    + **In Michelson:** `Left (Left (Right (Left (·))))`
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator



<a name="entrypoints-accept_ownership"></a>

---

### `accept_ownership`

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Accept_ownership` constructor.
    + **In Haskell:** `Accept_ownership (·)`
    + **In Michelson:** `Left (Left (Right (Right (·))))`
</details>
<p>



**Authorization:** The sender has to be `pending owner`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_PENDING_ADMIN`](#errors-NOT_PENDING_ADMIN) — Received an `accept_ownership` from an address other than what is in the pending owner field



<a name="entrypoints-migrate"></a>

---

### `migrate`

**Argument:** 
  + **In Haskell:** ***newAddress*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(address :newAddress)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Migrate` constructor.
    + **In Haskell:** `Migrate (·)`
    + **In Michelson:** `Left (Right (Left (Left (·))))`
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator



<a name="entrypoints-confirm_migration"></a>

---

### `confirm_migration`

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Confirm_migration` constructor.
    + **In Haskell:** `Confirm_migration (·)`
    + **In Michelson:** `Left (Right (Left (Right (·))))`
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

**Argument:** 
  + **In Haskell:** [`ProposeParams`](#types-ProposeParams) [`()`](#types-lparenrparen)
  + **In Michelson:** `(pair (nat %frozen_token) (unit %proposal_metadata))`
    + **Example:** <span id="example-id">`Pair 0 Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Propose` constructor.
    + **In Haskell:** `Propose (·)`
    + **In Michelson:** `Left (Right (Right (Left (·))))`
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

**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`VoteParam`](#types-VoteParam)
  + **In Michelson:** `(list (pair (bytes %proposal_key) (pair (bool %vote_type) (nat %vote_amount))))`
    + **Example:** <span id="example-id">`{ Pair 0x0a (Pair True 0) }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Vote` constructor.
    + **In Haskell:** `Vote (·)`
    + **In Michelson:** `Left (Right (Right (Right (·))))`
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

**Argument:** 
  + **In Haskell:** [`Natural`](#types-Natural)
  + **In Michelson:** `nat`
    + **Example:** <span id="example-id">`0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Set_voting_period` constructor.
    + **In Haskell:** `Set_voting_period (·)`
    + **In Michelson:** `Right (Left (Left (·)))`
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

**Argument:** 
  + **In Haskell:** [`Natural`](#types-Natural)
  + **In Michelson:** `nat`
    + **Example:** <span id="example-id">`0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Set_quorum_threshold` constructor.
    + **In Haskell:** `Set_quorum_threshold (·)`
    + **In Michelson:** `Right (Left (Right (Left (·))))`
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

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Flush` constructor.
    + **In Haskell:** `Flush (·)`
    + **In Michelson:** `Right (Left (Right (Right (·))))`
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

**Argument:** 
  + **In Haskell:** [`BurnParam`](#types-BurnParam)
  + **In Michelson:** `(pair (address %from_) (pair (nat %token_id) (nat %amount)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Burn` constructor.
    + **In Haskell:** `Burn (·)`
    + **In Michelson:** `Right (Right (Left (Left (·))))`
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

**Argument:** 
  + **In Haskell:** [`MintParam`](#types-MintParam)
  + **In Michelson:** `(pair (address %to_) (pair (nat %token_id) (nat %amount)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Mint` constructor.
    + **In Haskell:** `Mint (·)`
    + **In Michelson:** `Right (Right (Left (Right (·))))`
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator



<a name="entrypoints-transfer_contract_tokens"></a>

---

### `transfer_contract_tokens`

**Argument:** 
  + **In Haskell:** [`TransferContractTokensParam`](#types-TransferContractTokensParam)
  + **In Michelson:** `(pair (address %contract_address) (list %params (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" { Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0) } }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Transfer_contract_tokens` constructor.
    + **In Haskell:** `Transfer_contract_tokens (·)`
    + **In Michelson:** `Right (Right (Right (Left (·))))`
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`NOT_ADMIN`](#errors-NOT_ADMIN) — Received an operation that require administrative privileges from an address that is not the current administrator

* [`FAIL_TRANSFER_CONTRACT_TOKENS`](#errors-FAIL_TRANSFER_CONTRACT_TOKENS) — Trying to cross-transfer BaseDAO tokens to another contract that does not exist or is not a valid FA2 contract.



<a name="entrypoints-token_address"></a>

---

### `token_address`

**Argument:** 
  + **In Haskell:** [`ContractRef`](#types-Contract) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(contract address)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Token_address` constructor.
    + **In Haskell:** `Token_address (·)`
    + **In Michelson:** `Right (Right (Right (Right (·))))`
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
