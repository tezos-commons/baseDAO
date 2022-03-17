// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "types.mligo"
#include "common.mligo"
#include "error_codes.mligo"

(*
 * Auth checks for admin and store the address in parameter to the
 * `pending_owner` field in storage.
 *)
let transfer_ownership (param, store : transfer_ownership_param * storage) : return =
  let store = authorize_admin(store) in
  let store =
    if Tezos.self_address = param
    // If new admin is address of baseDAO, set as admin right away.
    then { store with admin = param ; }
    else { store with pending_owner = param ; }
  in (nil_op, store)

(*
 * Auth check for pending admin and copies the value in `pending_owner` field
 * to `admin` field. The `pending_owner` field is left with the address of the
 * new admin.
 *)
let accept_ownership(store : storage) : return =
  if store.pending_owner = Tezos.sender
  then (nil_op, { store with admin = Tezos.sender })
  else (failwith not_pending_admin : return)
