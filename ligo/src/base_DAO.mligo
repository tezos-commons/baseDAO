// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to BaseDAO.hs module

#include "common.mligo"
#include "defaults.mligo"
#include "token.mligo"

(*
 * Utility function to fetch and call an entrypoint lambda from startup_storage.
 * IMPORTANT: NO CHECKS ARE DONE HERE.
 *)
let call_stored_ep
  (ep_name, packed_param, full_store : string * bytes * full_storage)
    : return =
  let startup_store = full_store.0 in
  let config_store = full_store.1 in
  match Map.find_opt ep_name startup_store.stored_entrypoints with
  | Some lambda -> lambda(packed_param, config_store)
  | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
              ("ENTRYPOINT_NOT_FOUND", ()) : return)

(*
 * Returns name and packed parameter of a stored entrypoint that is part of FA2.
 *
 * Note: entrypoint parameters are packed as-is except for "Balance_of",
 * see 'startup.mligo' for more info.
 *)
let fa2_eps(param : fa2_parameter) : (string * bytes) =
  match param with
  | Transfer p -> ("transfer", Bytes.pack p)
  | Balance_of p ->
    let callback_address = Tezos.address(p.callback)
    in ("balance_of", Bytes.pack (p.requests, callback_address))
  | Update_operators p -> ("update_operators", Bytes.pack p)

(*
 * Returns name and packed parameter of a stored entrypoint that requires no xtz
 * to be transferred.
 * This checks for such xtz condition.
 *
 * Note: entrypoint parameters are packed as-is except for "GetVotePermitCounter"
 * and 'Vote`, see 'startup.mligo' for more info.
 *)
let requiring_no_xtz (param : forbid_xtz_params) : (string * bytes) =
  // check for no xtz
  if Tezos.amount > 0tez
  then ([%Michelson ({| { FAILWITH } |} : string * unit -> (string * bytes))]
        ("FORBIDDEN_XTZ", ()) : (string * bytes))
  else
    match param with
    | Call_FA2 p -> fa2_eps p
    | Drop_proposal p -> ("drop_proposal", Bytes.pack p)
    | Transfer_ownership p -> ("transfer_ownership", Bytes.pack p)
    | Accept_ownership p -> ("accept_ownership", Bytes.pack p)
    | Migrate p -> ("migrate", Bytes.pack p)
    | Confirm_migration p -> ("confirm_migration", Bytes.pack p)
    | Propose p -> ("propose", Bytes.pack p)
    | Vote p -> ("vote", Bytes.pack (p, Tezos.self_address))
    | Set_voting_period p -> ("set_voting_period", Bytes.pack p)
    | Set_quorum_threshold p -> ("set_quorum_threshold", Bytes.pack p)
    | Flush p -> ("flush", Bytes.pack p)
    | Burn p -> ("burn", Bytes.pack p)
    | Mint p -> ("mint", Bytes.pack p)
    | GetVotePermitCounter p ->
      let callback_address = Tezos.address(p.callback)
      in ("get_vote_permit_counter", Bytes.pack (p.param, callback_address))

(*
 * Returns name and packed parameter of a stored entrypoint that requires the
 * contract to not be migrated.
 * This checks for such migration condition.
 *)
let requiring_no_migration (param, store : migratable_parameter * storage) : (string * bytes) =
  // check not migrated
  let store = match store.migration_status with
    | Not_in_migration -> store
    | MigratingTo p_ -> store
    | MigratedTo (new_addr) ->
      ([%Michelson ({| { FAILWITH } |} : string * address -> storage)]
        ("MIGRATED", new_addr) : storage)
  in match param with
    // custom entrypoint that won't necessarily check for xtz
    | M_left p -> p
    // standard entrypoints that won't accept any xtz
    | M_right p -> requiring_no_xtz p

(*
 * The DAO contract after startup has terminated.
 * This will check for such startup condition, but still allow execution for 'admin'.
 *)
let running_contract
  (action, full_store : running_parameter * full_storage)
    : operation list * full_storage =
  let startup_store : startup_storage = full_store.0 in
  let store : storage = full_store.1.0 in
  let config : config = full_store.1.1 in
  if (startup_store.starting_up && not (Tezos.sender = store.admin))
  then (failwith "IN_STARTUP" : operation list * full_storage)
  else
    let (ep_name, packed_param) = match action with
      // entrypoints that require the contract to not be migrated
      | M_left p -> requiring_no_migration (p, store)
      // entrypoint that also work when the contract has been migrated
      | M_right p -> ("transfer_contract_tokens", Bytes.pack p)
    in
    let (ops, store) = call_stored_ep (ep_name, packed_param, full_store) in
    (ops, (startup_store, (store, config)))

(*
 * The DAO contract before startup has terminated.
 * This is basically the only entrypoint ('%startup') residing in the 'CODE'
 * section of the contract, as all the others are stored.
 * This will check for such startup condition and for the sender to be 'admin'.
 *)
let startup_contract
  (param, full_store : startup_parameter * full_storage)
    : operation list * full_storage =
  let startup_store : startup_storage = full_store.0 in
  let config_store : configured_storage = full_store.1 in
  let store_ = authorize_admin(config_store.0) in
  if startup_store.starting_up
  then
    let startup_store = match param with
      | Some upd_param ->
        begin
          let (ep_name, upd_type) = upd_param in
          let upd = match upd_type with
            | Some upd_bytes ->
              begin
              match (Bytes.unpack upd_bytes : storable_entrypoint option) with
                | Some upd_lam -> Some upd_lam
                | None -> (failwith "NOT_EP_LAMBDA" : storable_entrypoint option)
              end
            | None -> (None : (storable_entrypoint option))
          in
          let ep_map = Map.update ep_name upd startup_store.stored_entrypoints in
          {startup_store with stored_entrypoints = ep_map}
        end
      | None ->
        {startup_store with starting_up = false}
    in (nil_op, (startup_store, config_store))
  else (failwith "NOT_IN_STARTUP" : operation list * full_storage)

(*
 * The actual DAO contract, which in this version is the same independently from
 * the DAO logic.
 *)
let base_DAO_contract (param, full_store : parameter * full_storage)
    : operation list * full_storage =
  let startup_store : startup_storage = full_store.0 in
  let store : storage = full_store.1.0 in
  let config : config = full_store.1.1 in
  match param with
    | M_left p -> startup_contract (p, full_store)
    | M_right p -> running_contract (p, full_store)
