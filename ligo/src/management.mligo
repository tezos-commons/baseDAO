// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Management.hs module

#include "types.mligo"
#include "common.mligo"

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
    then (([] : operation list), { store with admin = Tezos.sender })
    else
      ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
        ("NOT_PENDING_ADMIN", ()) : return)

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
    Not_in_migration ->
      ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
        ("NOT_MIGRATING", ()) : return)
  | MigratingTo (new_addr) -> if new_addr = Tezos.sender
      then (([] : operation list), { store with migration_status = MigratedTo (new_addr) })
      else
        ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
          ("NOT_MIGRATION_TARGET", ()) : return)
  | MigratedTo (new_addr)  ->
      ([%Michelson ({| { FAILWITH } |} : string * address -> return)]
        ("MIGRATED", new_addr) : return)
