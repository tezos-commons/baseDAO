// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Management.hs module

#include "types.mligo"
#include "common.mligo"

[@inline]
let ensure_not_migrated (storage : storage): storage =
  match storage.migration_status with
      Not_in_migration -> storage
    | MigratingTo (p) -> storage
    | MigratedTo (new_addr) ->
        ([%Michelson ({| { FAILWITH } |} : string * address -> storage)]
          ("MIGRATED", new_addr) : storage)

(*
 * Auth checks for admin and store the address in parameter to the
 * 'pending_owner' field in storage.
 *)
let transfer_ownership
    (param, store : transfer_ownership_param * storage) : return =
      let store = authorize_admin(store) in
        (([] : operation list), { store with pending_owner = param ; })

(*
 * Auth check for pending admin and copies the value in 'pending_owner' field
 * to 'admin' field. The 'pending_owner' field is left with the address of the
 * new admin.
 *)
let accept_ownership(param, store : unit * storage) : return =
  if store.pending_owner = Tezos.sender
    then (([] : operation list), { store with admin = store.pending_owner })
    else (failwith("NOT_PENDING_ADMIN"): return)

(*
 * Auth check for admin and sets the migration status to 'MigratingTo' using
 * the address from parameter, overwriting any address from any previous
 * 'Migrate' calls.
 *)
let migrate(param, store : migrate_param * storage) : return =
  let store = authorize_admin(store) in
    (([] : operation list), { store with migration_status = MigratingTo (param) })

(*
 * Auth check for new contract and sets the migration_status to 'Migrated', using
 * the address from the earlier value in the migration_status field.
 *)
let confirm_migration(param, store : unit * storage) : return =
  match store.migration_status with
    Not_in_migration -> (failwith("NOT_MIGRATING"): return)
  | MigratingTo (new_addr) -> if new_addr = Tezos.sender
      then (([] : operation list), { store with migration_status = MigratedTo (new_addr) })
      else (failwith("NOT_MIGRATION_TARGET"): return)
  | MigratedTo (new_addr)  ->
    ([%Michelson ({| { FAILWITH } |} : string * address -> return)]
      ("MIGRATED", new_addr) : return)

(*
 * Call a custom entrypoint. Looks up the packed code for entrypoint using the entrypoint
 * name in the parameter. Then unpacks the code to a lambda of type 'bytes -> return'.
 * Then just pass the bytes from parameter to this lambda return the resulting value.
 *
 * NO AUTH CHECKS ARE DONE.
 *)
let call_custom(param, full_storage : custom_ep_param * full_storage) : return_with_full_storage =
  let ep_name = param.0 in
  let config = full_storage.1 in
  let storage = full_storage.0 in
  let packed_param = param.1 in

  match Map.find_opt ep_name config.custom_entrypoints with
    Some (ep_code) ->
      begin
        match ((Bytes.unpack ep_code) : ((bytes * full_storage) -> return_with_full_storage) option) with
          Some lambda -> lambda (packed_param, full_storage)
        | None -> (failwith("UNPACKING_FAILED"): return_with_full_storage)
      end
  | None -> (failwith("ENTRYPOINT_NOT_FOUND"): return_with_full_storage)
