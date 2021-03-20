// SPDX-FileCopyrightText: 2020 TQ Tezos
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
  if Tezos.amount > 0tez
  then (failwith("FORBIDDEN_XTZ") : return)
  else
    match param with
    | Call_FA2 (p) -> call_fa2(p, store)
    | Drop_proposal (p) -> drop_proposal(p, config, store)
    | Transfer_ownership (p) -> transfer_ownership(p, store)
    | Accept_ownership (p) -> accept_ownership(p, store)
    | Migrate (p) -> migrate(p, store)
    | Confirm_migration (p) -> confirm_migration(p, store)
    | Vote (p) -> vote(p, config, store)
    | Set_fixed_fee_in_token (p) -> set_fixed_fee_in_token(p, store)
    | Set_voting_period (p) -> set_voting_period(p, config, store)
    | Set_quorum_threshold (p) -> set_quorum_threshold(p, config, store)
    | Flush (p) -> flush (p, config, store)
    | Burn (p) -> burn(p, store)
    | Mint (p) -> mint(p, store)
    | Get_vote_permit_counter (p) -> get_vote_permit_counter(p, store)
    | Get_total_supply (p) -> get_total_supply(p, store)
    | Freeze p -> freeze(p, config, store)
    | Unfreeze p -> unfreeze(p, config, store)


(*
 * Entrypoints that allow xtz to be sent.
 *)
let allowing_xtz (param, store, config : allow_xtz_params * storage * config) =
  match param with
    | CallCustom p -> call_custom(p, store, config)
    | Propose (p) -> propose(p, config, store)

(*
 * Entrypoints that require the contract to be migrated.
 * Note: This checks for such migration condition.
 *)
let requiring_no_migration (param, store, config : migratable_parameter * storage * config)
    : operation list * storage =
  // check not migrated
  let store = match store.migration_status with
    | Not_in_migration -> store
    | MigratingTo p_ -> store
    | MigratedTo (new_addr) ->
      ([%Michelson ({| { FAILWITH } |} : string * address -> storage)]
        ("MIGRATED", new_addr) : storage)
  in match param with
    // entrypoints that won't necessarily check for xtz
    | M_left p -> allowing_xtz(p, store, config)
    // standard entrypoints that won't accept any xtz
    | M_right p -> requiring_no_xtz(p, store, config)

(*
 * The actual DAO contract, which in this version is the same independently from
 * the DAO logic.
 *)
let base_DAO_contract (param, full_store : parameter * full_storage)
    : operation list * full_storage =
  let store : storage = full_store.0 in
  let config : config = full_store.1 in
  let (ops, new_store) = match param with
    // entrypoints that require the contract to not be migrated
    | M_left p -> requiring_no_migration (p, store, config)
    // entrypoint that also work when the contract has been migrated
    | M_right p -> transfer_contract_tokens(p, store)
  in (ops, (new_store, config))
