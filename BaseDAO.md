# FA2 smart contract

This documentation describes a smart contract which implements FA2 spec
(https://gitlab.com/tzip/tzip/-/blob/667f925471728bb8e81fbd30b93a171e401b6dc1/proposals/tzip-12/tzip-12.md)


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
    + **In Michelson:** `Left (Left (·))`
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
    + **In Michelson:** `Left (Left (·))`
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
    + **In Michelson:** `Left (Left (·))`
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
    + **In Michelson:** `Left (Left (·))`
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
    + **In Michelson:** `Left (Left (·))`
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
    + **In Michelson:** `Left (Right (·))`
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMINISTRATOR`](#errors-NOT_ADMINISTRATOR) — Received an operation that require administrative privileges from an address that is not the current administrator



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
    + **In Michelson:** `Right (Left (·))`
</details>
<p>



**Authorization:** The sender has to be `pending owner`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NO_PENDING_ADMINISTRATOR_SET`](#errors-NO_PENDING_ADMINISTRATOR_SET) — Received an `accept_ownership` call when no pending owner was set

* [`NOT_PENDING_ADMINISTRATOR`](#errors-NOT_PENDING_ADMINISTRATOR) — Received an `accept_ownership` from an address other than what is in the pending owner field



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
    + **In Michelson:** `Right (Right (Left (·)))`
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`MIGRATED`](#errors-MIGRATED) — Recieved a call on a migrated contract

* [`NOT_ADMINISTRATOR`](#errors-NOT_ADMINISTRATOR) — Received an operation that require administrative privileges from an address that is not the current administrator



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
    + **In Michelson:** `Right (Right (Right (·)))`
</details>
<p>



**Authorization:** The sender has to be `migration target contract`.

**Possible errors:**
* [`NOT_MIGRATING`](#errors-NOT_MIGRATING) — Recieved a confirm_migration call on a contract that is not in migration

* [`NOT_MIGRATION_TARGET`](#errors-NOT_MIGRATION_TARGET) — Recieved a confirm_migration call on a contract from an address other than the new version

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



<a name="types-Text"></a>

---

### `Text`

Michelson string.

This has to contain only ASCII characters with codes from [32; 126] range; additionally, newline feed character is allowed.

**Final Michelson representation:** `string`



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

<a name="errors-MIGRATED"></a>

---

### `MIGRATED`

**Class:** Action exception

**Fires if:** Recieved a call on a migrated contract

**Representation:** `("MIGRATED", <error argument>)`.

Provided error argument will be of type [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen).

<a name="errors-NOT_ADMINISTRATOR"></a>

---

### `NOT_ADMINISTRATOR`

**Class:** Action exception

**Fires if:** Received an operation that require administrative privileges from an address that is not the current administrator

**Representation:** `("NOT_ADMINISTRATOR", ())`.

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

<a name="errors-NOT_PENDING_ADMINISTRATOR"></a>

---

### `NOT_PENDING_ADMINISTRATOR`

**Class:** Action exception

**Fires if:** Received an `accept_ownership` from an address other than what is in the pending owner field

**Representation:** `("NOT_PENDING_ADMINISTRATOR", ())`.

<a name="errors-NO_PENDING_ADMINISTRATOR_SET"></a>

---

### `NO_PENDING_ADMINISTRATOR_SET`

**Class:** Action exception

**Fires if:** Received an `accept_ownership` call when no pending owner was set

**Representation:** `("NO_PENDING_ADMINISTRATOR_SET", ())`.
