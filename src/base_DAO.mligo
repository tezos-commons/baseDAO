// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "parameter.mligo"
#include "implementation.mligo"
#include "management.mligo"
#include "permit.mligo"
#include "proposal.mligo"
#include "defaults.mligo"
#include "token.mligo"
#include "error_codes.mligo"

(*
 * Entrypoints that require no xtz to be sent.
 * This checks for such xtz condition.
 *)
let requiring_no_xtz (param, store, config : forbid_xtz_params * storage * config)
    : operation list * storage =
  // check for no xtz
  if Tezos.amount > 0tez then
    (failwith forbidden_xtz : return)
  else
    match param with
    | Drop_proposal (p)      -> drop_proposal(p, config, store)
    | Vote (p)               -> vote(p, config, store)
    | Flush (p)              -> flush (p, config, store)
    | Freeze p               -> freeze(p, config, store)
    | Unfreeze p             -> unfreeze(p, config, store)
    | Update_delegate p      -> update_delegates(p, store)
    | Unstake_vote p         -> unstake_vote(p, config, store)

(*
 * Entrypoints that allow xtz to be sent.
 *)
let allowing_xtz_contract (param, store, config : allow_xtz_params_contract * storage * config) =
  match param with
  | Propose (p)                -> propose(p, config, store)
  | Transfer_contract_tokens p -> transfer_contract_tokens(p, store)
  | Transfer_ownership (p)     -> transfer_ownership(p, store)
  | Accept_ownership           -> accept_ownership(store)
  | Default _                  -> (([] : operation list), store)

let allowing_xtz (param, store, config : allow_xtz_params * storage * config) =
  match param with
  | M_left p -> allowing_xtz_contract (p, store, config)
  | M_right p -> custom_ep (p, store, config)

(*
 * The actual DAO contract, which is the same, regardless of the specific DAO
 * logic included.
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
