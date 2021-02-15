// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Token.hs module

#include "common.mligo"
#include "types.mligo"
#include "token/fa2.mligo"

let burn(param, store : burn_param * storage) : return =
  let store = authorize_admin(store) in
  let (ledger, total_supply) = debit_from (param.amount, param.from_, param.token_id, store.ledger, store.total_supply)
  in (([] : operation list), { store with ledger = ledger; total_supply = total_supply})

let mint(param, store : mint_param * storage) : return =
  let store = authorize_admin(store) in
  let (ledger, total_supply) = credit_to (param.amount, param.to_, param.token_id, store.ledger, store.total_supply)
  in (([] : operation list), {store with ledger = ledger; total_supply = total_supply})

let transfer_contract_tokens
    (param, store : transfer_contract_tokens_param * storage) : return =
  let store = authorize_admin(store) in
  match (Tezos.get_entrypoint_opt "%transfer" param.contract_address
      : transfer_params contract option) with
    Some contract ->
      let transfer_operation = Tezos.transaction param.params 0mutez contract
      in (([transfer_operation] : operation list), store)
  | None ->
      ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
        ("FAIL_TRANSFER_CONTRACT_TOKENS", ()) : return)

