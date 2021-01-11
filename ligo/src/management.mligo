// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Management.hs module

#include "types.mligo"

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
