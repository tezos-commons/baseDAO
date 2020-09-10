<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->

# Overview

This specification is based on the following documents:

- [BaseDAO](https://docs.google.com/document/d/1bTMOntqvxbHmjaERqETmqw0hy2f8JdVsjNyhpWUOqDo/edit#)

- Tezos Token Standard: [FA2][FA2]

The contract described here consists of two parts:
- Token functionality (FA2-based).
- DAO functionality with proposals and voting.

These two parts are coupled into one smart contract because interaction between smart contracts in Tezos is expensive and hard to get right.

# General Requirements

- The contract must be FA2 compatible as to facilitate listing on
  exchanges and interaction with services which support FA2.

- The contract must store tokens of two types: frozen and unfrozen.

- The storage of the contract must have annotations for all fields
  and must be documented to make its interpretation easy for users.

# State model

This chapter provides a high-level overview of the contract's state.
Note that the actual storage type is implementation detail and is not specified here.

**TODO**

# Errors

In error scenarios the stablecoin contract fails with a string or a pair where the first item is a string.
Here is a summary of all the strings used as error messages.
We start with standard FA2 errors which are part of the FA2 specification.

| Error                      | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `FA2_TOKEN_UNDEFINED`      | One of the specified `token_id`s is not defined                             |
| `FA2_INSUFFICIENT_BALANCE` | Cannot debit from a wallet because of excessive amount of tokens            |
| `FA2_NOT_OPERATOR`         | A transfer is initiated neither by the token owner nor a permitted operator |

The next group consists of the errors that are not part of the FA2 specification.

| Error                        | Description |
|------------------------------|-------------|
| `QUORUM_NOT_MET`             | **TODO**    |

# Entrypoints

Full list:
* [`transfer`](#transfer)
* [`balance_of`](#balance_of)
* [`token_metadata_registry`](#token_metadata_registry)
* [`update_operators`](#update_operators)
* [`is_operator`](#is_operator)

Format:
```
**entrypoint_name**

<optional pseudocode description of the parameter type>
Parameter (in Michelson): X

<description>
```

* Top-level contract parameter type MUST have all entrypoints listed below.
* Each entrypoint MUST be callable using the standard entrypoints machinery of Michelson by specifying **entrypoint_name** and a value of the type `X` (its argument).
* The previous bullet point implies that each `X` must have a field annotations with the corresponding entrypoint name.
In the definitions below it may be omitted, but it is still implied.

Pseudocode is semi-formally defined as a list of assignments where the last assignment has `entrypoint_name` on the left and its argument type (that maps to `X`) on the right.

Note: pseudocode is provided only for readability.
If Michelson type contradics what's written in pseudocode, the Michelson defition takes precedence.

## Standard FA2 Token Functions

Functions present in the [*FA2 Tezos Token Standard*][FA2].

### **transfer**

Types
```
token_id = nat

transfer_destination =
  ( address :to_
  , token_id :token_id
  , nat :amount
  )

transfer_param =
  ( address :from_
  , list transfer_destination :txs
  )

transfer = list transfer_param
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

- Althought the contract supports two types of tokens: frozen token (`token_id = 1`) and unfrozen token (`token_id = 0`), all `token_id` values passed to this entrypoint MUST be 0.

- If the destination address is the `freeze_my_tokens` address:
  - This entrypoint MUST update the unfrozen token balance(`token_id = 0`) according to FA2 requirement.
  - It MUST also update the frozen token balance (`token_id = 1`) of the source address by the amount specified in the parameter.

### **balance_of**

Types
```
token_id = nat

balance_of_request =
  ( address :owner
  , token_id :token_id
  )

balance_of_response =
  ( balance_of_request :request
  , nat :balance
  )

balance_of =
  ( list balance_of_request :requests
  , contract (list balance_of_response) :callback
  )
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

Types
```
token_metadata_registry is contract (address)
```

Parameter (in Michelson)
```
(contract %token_metadata_registry address)
```

- Return contract address that holds token metadata.
- Since the contract owns its token metadata the returned value of this entrypoint call will always be equal to `SELF`.

### **update_operators**

Types
```
operator_param =
  ( address         :owner
  , address         :operator
  )

update_operator_param =
  | Add_operator    operator_param
  | Remove_operator operator_param

update_operators = list update_operator_param
```

Parameter (in Michelson)
```
(list %update_operators
  (or
    (pair %add_operator
      (address %owner)
      (address %operator)
    )
    (pair %remove_operator
      (address %owner)
      (address %operator)
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

- Each `owner` must be equal to `SENDER`, otherwise `NOT_TOKEN_OWNER` error occurs.

### **is_operator**

Types
```
operator_param =
  ( address         :owner
  , address         :operator
  )

is_operator_response =
  ( operator_param :operator
  , bool           :is_operator
  )

is_operator =
  ( operator_param                :operator
  , contract is_operator_response :callback
  )
```

Parameter (in Michelson):
```
(pair %is_operator
  (pair %operator
    (address %owner)
    (address %operator)
  )
  (contract %callback
    (pair
      (pair %operator
        (address %owner)
        (address %operator)
      )
      (bool %is_operator)
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

## Custom (non-FA2) token functions

Functions related to token transfers, but not present in FA2. They do not have `token_id` argument because only unfrozen token is supported (`token_id = 0`) and not necessary (since they are not part of FA2).

#### **mint**

Types
```
mint = list nat
```

Parameter (in Michelson):
```
list nat
```

- Produces the given amounts of tokens to the wallet associated with a predefined address.
- Each minting must happen atomically, so if one of them fails, then the whole operation must fail.
- Fails if the sender is not administrator.

#### **burn**

Types
```
burn = list nat
```

Parameter (in Michelson):
```
list nat
```

- Decreases balance of a wallet associated with a predefined address.
- Each burning operation must happen atomically, so if one of them fails, then the whole operation must fail.
- Fails if the sender is not administrator.

## Proposal entrypoints

**TODO**

[FA2]: https://gitlab.com/tzip/tzip/-/blob/3a6464b1e641008b77a83807a0c102e7602c6af4/proposals/tzip-12/tzip-12.md
