// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Token/FA2.hs module

#include "../types.mligo"

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
let debit_from (amt, from_, token_id, ledger, total_supply: nat * address * token_id * ledger * total_supply): (ledger * total_supply) =
  match Big_map.find_opt (from_, token_id) ledger with
    Some bal ->

      let new_total_supply =
        match Map.find_opt token_id total_supply with
          Some current_total_supply ->
            (match Michelson.is_nat (current_total_supply - amt) with
              Some new_total_supply ->
                new_total_supply
            | None ->
                (failwith("NEGATIVE_TOTAL_SUPPLY") : nat)
            )
        | None ->
            (failwith("FA2_TOKEN_UNDEFINED") : nat)

      in (match Michelson.is_nat (bal - amt) with
            Some new_bal ->
              let ledger = Big_map.update (from_, token_id) (Some new_bal) ledger in
              let total_supply = Map.update token_id (Some new_total_supply) total_supply in
              (ledger, total_supply)
          | None ->
              ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> (ledger * total_supply))] ("FA2_INSUFFICIENT_BALANCE", (amt, bal)) : (ledger * total_supply))
         )

  | None ->
      if (amt = 0n) then (ledger, total_supply) // We allow 0 transfer
      else
        ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> (ledger * total_supply))] ("FA2_INSUFFICIENT_BALANCE", (amt, 0n)) : (ledger * total_supply))

[@inline]
let credit_to (amt, to_, token_id, ledger, total_supply : nat * address * nat * ledger * total_supply): (ledger * total_supply) =
  match Map.find_opt token_id total_supply with
    Some current_total_supply ->
      let new_bal =
        match Big_map.find_opt (to_, token_id) ledger with
          Some bal -> bal + amt
        | None -> amt
      in  let ledger = Big_map.update (to_, token_id) (Some new_bal) ledger in
          let total_supply = Map.update token_id (Some (current_total_supply + amt)) total_supply in
          (ledger, total_supply)
  | None ->
      (failwith("FA2_TOKEN_UNDEFINED") : (ledger * total_supply))

// -----------------------------------------------------------------
// Transfer
// -----------------------------------------------------------------

let transfer_item (store, ti : storage * transfer_item): storage =
  let transfer_one (store, tx : storage * transfer_destination): storage =
    let valid_from_ = check_sender (ti.from_, store) in
    if tx.amount = 0n then store
    else (
      let valid_token_id =
        if tx.token_id = store.unfrozen_token_id then tx.token_id
        else if tx.token_id = store.frozen_token_id then (
          if sender = store.admin then tx.token_id
          else
            (failwith("FROZEN_TOKEN_NOT_TRANSFERABLE") : token_id)
        ) else
            ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
              ("FA2_TOKEN_UNDEFINED", ()) : token_id)
      in
      let ledger = store.ledger in
      let total_supply = store.total_supply in
      let (ledger, total_supply) = debit_from(tx.amount, valid_from_, valid_token_id, ledger, total_supply) in
      let (ledger, total_supply) = credit_to(tx.amount, tx.to_, valid_token_id, ledger, total_supply) in
      { store with
        ledger = ledger
      ; total_supply = total_supply
      }
    )
  in List.fold transfer_one ti.txs store

let transfer (params, store : transfer_params * storage): return =
  let store = List.fold transfer_item params store in
  (([] : operation list), store)


// -----------------------------------------------------------------
// Balance of
// -----------------------------------------------------------------

[@inline]
let validate_token_type (token_id, unfrozen_token_id, frozen_token_id : token_id * nat * nat): token_id =
  if (token_id = unfrozen_token_id || token_id = frozen_token_id) then
    token_id
  else ([%Michelson ({| { FAILWITH } |} : string * unit -> token_id)]
          ("FA2_TOKEN_UNDEFINED", ()) : token_id)

let balance_of (params, store : balance_request_params * storage): return =
  let check_one (req : balance_request_item): balance_response_item =
    let valid_token_id = validate_token_type(req.token_id, store.unfrozen_token_id, store.frozen_token_id) in
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
let validate_operator_token (operator_param, unfrozen_token_id, frozen_token_id : operator_param * nat * nat): operator_param =
  let token_id = operator_param.token_id in
  if (token_id = unfrozen_token_id) then
    operator_param
  else if (token_id = frozen_token_id) then
    (failwith("OPERATION_PROHIBITED") : operator_param)
  else
    (failwith("FA2_TOKEN_UNDEFINED") : operator_param)

let update_one (store, param: storage * update_operator): storage =
  let (operator_update, operator_param) =
    match param with
      Add_operator p -> (Some unit, p)
    | Remove_operator p -> ((None : unit option), p)
  in
  let operator_param = validate_operator_token (operator_param, store.unfrozen_token_id, store.frozen_token_id) in
  if (sender = operator_param.owner) then
    let key: operator = { owner = operator_param.owner; operator = operator_param.operator} in
    let updated_operators = Big_map.update key operator_update store.operators
    in  { store with
          operators = updated_operators
        }
  else
    (failwith("NOT_OWNER") : storage)

let update_operators (params, store : update_operators_param * storage):return =
  let store = List.fold update_one params store in
  (([] : operation list), store)

