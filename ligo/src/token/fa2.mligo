// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Token/FA2.hs module

#include "../types.mligo"


let unfrozen_token_id: nat = 0n
let frozen_token_id: nat = 1n

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let check_sender (from_ , store : address * storage): address =
  if (sender = store.admin) then from_
  else if (sender = from_) then from_
  else
    let key: operator = { owner = from_; operator = sender} in
    if Big_map.mem key store.operators then
      from_
    else
     ([%Michelson ({| { FAILWITH } |} : string * unit -> address)]
        ("FA2_NOT_OPERATOR", ()) : address)

[@inline]
let debit_from (amt, from_, token_id, ledger: nat * address * token_id * ledger): ledger =
  match Big_map.find_opt (from_, token_id) ledger with
    Some bal ->
      begin
        match Michelson.is_nat (bal - amt) with
          Some new_bal -> Big_map.update (from_, token_id) (Some new_bal) ledger
        | None ->
            ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> ledger)] ("FA2_INSUFFICIENT_BALANCE", (amt, bal)) : ledger)
      end
  | None ->
      if (amt = 0n) then ledger // We allow 0 transfer
      else
        ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> ledger)] ("FA2_INSUFFICIENT_BALANCE", (amt, 0n)) : ledger)

[@inline]
let credit_to (amt, to_, token_id, ledger : nat * address * nat * ledger): ledger =
  let new_bal =
    match Big_map.find_opt (to_, token_id) ledger with
      Some bal -> bal + amt
    | None -> amt
  in Big_map.update (to_, token_id) (Some new_bal) ledger

// -----------------------------------------------------------------
// Transfer
// -----------------------------------------------------------------

let transfer_item (store, ti : storage * transfer_item): storage =
  let transfer_one (store, tx : storage * transfer_destination): storage =
    let valid_from_ = check_sender (ti.from_, store) in
    if tx.amount = 0n then store
    else (
      let valid_token_id =
        if tx.token_id = unfrozen_token_id then tx.token_id
        else if tx.token_id = frozen_token_id then (
          if sender = store.admin then tx.token_id
          else
            ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
              ("FROZEN_TOKEN_NOT_TRANSFERABLE", ()) : token_id)
        ) else
            ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
              ("FA2_TOKEN_UNDEFINED", ()) : token_id)
      in
      let ledger = store.ledger in
      let ledger = debit_from(tx.amount, valid_from_, valid_token_id, ledger) in
      let ledger = credit_to(tx.amount, tx.to_, valid_token_id, ledger) in
      { store with ledger = ledger }
    )
  in List.fold transfer_one ti.txs store

let transfer (params, store : transfer_params * storage): return =
  let store = List.fold transfer_item params store in
  (([] : operation list), store)


// -----------------------------------------------------------------
// Balance of
// -----------------------------------------------------------------

[@inline]
let validate_token_type (token_id : token_id): token_id =
  if (token_id = unfrozen_token_id || token_id = frozen_token_id) then
    token_id
  else ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
          ("FA2_TOKEN_UNDEFINED", ()) : token_id)

let balance_of (params, store : balance_request_params * storage): return =
  let check_one (req : balance_request_item): balance_response_item =
    let valid_token_id = validate_token_type(req.token_id) in
    let bal =
      match Big_map.find_opt (req.owner, valid_token_id) store.ledger with
        Some bal -> bal
      | None -> 0n
    in { request = req; balance = bal}
  in
  let result = List.map check_one params.requests in
  let transfer_operation = Tezos.transaction result 0mutez params.callback
  in (([transfer_operation] : operation list), store)

// -----------------------------------------------------------------
// Update operators entrypoint
// -----------------------------------------------------------------
[@inline]
let validate_operator_token (token_id : token_id): token_id =
  if (token_id = unfrozen_token_id) then
    token_id
  else if (token_id = frozen_token_id) then
    ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
      ("OPERATION_PROHIBITED", ()) : token_id)
  else
    ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
      ("FA2_TOKEN_UNDEFINED", ()) : token_id)

let update_one (store, param: storage * update_operator): storage =
  let (operator_update, operator_param) =
    match param with
      Add_operator p -> (Some unit, p)
    | Remove_operator p -> ((None : unit option), p)
  in
  let valid_token_id = validate_operator_token (operator_param.token_id) in
  if (sender = operator_param.owner) then
    let key: operator = { owner = operator_param.owner; operator = operator_param.operator} in
    let updated_operators = Big_map.update key operator_update store.operators
    in  { store with
          operators = updated_operators
        }
  else
    ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
      ("NOT_OWNER", ()) : storage)

let update_operators (params, store : update_operators_param * storage):return =
  let store = List.fold update_one params store in
  (([] : operation list), store)

