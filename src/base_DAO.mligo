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
let requiring_no_xtz (param, store: forbid_xtz_params * storage)
    : operation list * storage =
  // check for no xtz
  if Tezos.get_amount unit > 0tez then
    (failwith forbidden_xtz : return)
  else
    match param with
    | Drop_proposal (p)      -> drop_proposal(p, store)
    | Vote (p)               -> vote(p, store)
    | Flush (p)              -> flush (p, store)
    | Freeze p               -> freeze(p, store)
    | Unfreeze p             -> unfreeze(p, store)
    | Update_delegate p      -> update_delegates(p, store)
    | Unstake_vote p         -> unstake_vote(p, store)

(*
 * Entrypoints that allow xtz to be sent.
 *)
let allowing_xtz_contract (param, store: allow_xtz_params_contract * storage) =
  match param with
  | Propose (p)                -> propose(p, store)
  | Transfer_contract_tokens p -> transfer_contract_tokens(p, store)
  | Transfer_ownership (p)     -> transfer_ownership(p, store)
  | Accept_ownership           -> accept_ownership(store)
  | Default _                  -> (([] : operation list), store)

let allowing_xtz (param, store: allow_xtz_params * storage) =
  match param with
  | M_left p -> allowing_xtz_contract (p, store)
  | M_right p -> custom_ep (p, store)

(*
 * The actual DAO contract, which is the same, regardless of the specific DAO
 * logic included.
 *)
let base_DAO_contract (param, store : parameter * storage)
    : operation list * storage =
  let (ops, new_store) = match param with
    // An entrypoint that allows tz to be sent was called
    | M_left p -> allowing_xtz (p, store)
    // An entrypoint that require no tz to be sent was called
    | M_right p -> requiring_no_xtz(p, store)
  in (ops, new_store)
