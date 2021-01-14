// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Token.hs module

#include "types.mligo"
#include "token/fa2.mligo"

let call_fa2(param, store : fa2_parameter * storage) : return =
  match param with
    Transfer (p) -> transfer (p, store)
  | Balance_of (p) -> balance_of(p, store)
  | Token_metadata_registry (p) -> token_metadata_registry(p, store)
  | Update_operators (p) -> update_operators(p, store)

let authorize_admin (store : storage): storage =
  if sender = store.admin then
    store
  else
    (failwith("NOT_ADMIN"): storage)

let burn(param, store : burn_param * storage) : return =
  let store = authorize_admin(ensure_not_migrated(store)) in
  let store = debit_from (param.amount, param.from_, param.token_id, store)
  in (([] : operation list), store)

let mint(param, store : mint_param * storage) : return =
  let store = authorize_admin(ensure_not_migrated(store)) in
  let store = credit_to (param.amount, param.to_, param.token_id, store)
  in (([] : operation list), store)

let transfer_contract_tokens
    (param, store : transfer_contract_tokens_param * storage) : return =
  let store = authorize_admin(store) in
  match (Tezos.get_entrypoint_opt "%transfer" param.contract_address
      : transfer_params contract option) with
    Some contract ->
      let transfer_operation = Tezos.transaction param.params 0mutez contract
      in (([transfer_operation] : operation list), store)
  | None ->
      (failwith("FAIL_TRANSFER_CONTRACT_TOKENS"): return)

