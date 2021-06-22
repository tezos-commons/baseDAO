// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to BaseDAO.hs module

#include "management.mligo"
#include "permit.mligo"
#include "proposal.mligo"
#include "defaults.mligo"
#include "token.mligo"

(*
 * Entrypoints that require the no xtz to be sent.
 * This checks for such xtz condition.
 *)
let requiring_no_xtz (param, store, config : forbid_xtz_params * storage * config)
    : operation list * storage =
  // check for no xtz
  if Tezos.amount > 0tez then
    (failwith("FORBIDDEN_XTZ") : return)
  else
    match param with
    | Drop_proposal (p)      -> drop_proposal(p, config, store)
    | Vote (p)               -> vote(p, config, store)
    | Flush (p)              -> flush (p, config, store)
    | Freeze p               -> freeze(p, config, store)
    | Unfreeze p             -> unfreeze(p, config, store)
    | Update_delegate p      -> update_delegates(p, store)


(*
 * Entrypoints that allow xtz to be sent.
 *)
let allowing_xtz (param, store, config : allow_xtz_params * storage * config) =
  match param with
  | CallCustom p               -> call_custom(p, store, config)
  | Propose (p)                -> propose(p, config, store)
  | Transfer_contract_tokens p -> transfer_contract_tokens(p, store)
  | Transfer_ownership (p)     -> transfer_ownership(p, store)
  | Accept_ownership           -> accept_ownership(store)

(*
 * The actual DAO contract, which in this version is the same independently from
 * the DAO logic.
 *)
let base_DAO_contract (param, full_store : parameter * full_storage)
    : operation list * full_storage =
  let store : storage = full_store.0 in
  let config : config = full_store.1 in
  let (ops, new_store) = match param with
    // An entrypoint that allows tz to be sent was called
    | M_left p -> allowing_xtz (p, store, config)
    // An entrypoint that require no tz to be sent was called
    | M_right p -> requiring_no_xtz(p, store, config)
  in (ops, (new_store, config))
