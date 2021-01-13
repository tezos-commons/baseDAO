// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Management.hs module

#include "types.mligo"


// -----------------------------------------------------------------
// Shared Function
// -----------------------------------------------------------------
#if !MANAGEMENT_H
#define MANAGEMENT_H

[@inline]
let ensure_not_migrated (storage : storage): storage =
  match storage.migration_status with
      Not_in_migration -> storage
    | MigratingTo (p) -> storage
    | MigratedTo (p) -> (failwith ("MIGRATED") : storage)

let authorize_admin (storage : storage): storage =
  if storage.admin = Tezos.sender
  then storage
  else (failwith("NOT_ADMIN") : storage)

#endif  // MANAGEMENT_H included


let transfer_ownership
    (param, store : transfer_ownership_param * storage) : return =
  not_implemented("transfer_ownership")

let accept_ownership(param, store : unit * storage) : return =
  not_implemented("transfer_ownership")

let migrate(param, store : migrate_param * storage) : return =
  not_implemented("migrate")

let confirm_migration(param, store : unit * storage) : return =
  not_implemented("confirm_migration")

let call_custom(param, store : custom_ep_param * storage) : return =
  not_implemented("call_custom")
