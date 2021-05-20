// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"
#include "common.mligo"

(*
 * Auth checks for admin and store the address in parameter to the
 * 'pending_owner' field in storage.
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
 * Auth check for pending admin and copies the value in 'pending_owner' field
 * to 'admin' field. The 'pending_owner' field is left with the address of the
 * new admin.
 *)
let accept_ownership(store : storage) : return =
  if store.pending_owner = Tezos.sender
  then (nil_op, { store with admin = Tezos.sender })
  else (failwith("NOT_PENDING_ADMIN") : return)

(*
 * Call a custom entrypoint.
 *
 * First it looks up the packed code for the entrypoint using the entrypoint
 * name in the parameter.
 * Then it unpacks the code to a lambda of type '(bytes * full_storage) -> return'.
 * Finally it executes this lambda, with the bytes from the function parameter,
 * and returns the resulting value.
 *
 * NO AUTH CHECKS ARE DONE.
 *)
let call_custom(param, store, config : custom_ep_param * storage * config) : return =
  let ep_name = param.0 in
  let packed_param = param.1 in

  let packed_ep =
    match Map.find_opt ep_name config.custom_entrypoints with
    | Some (ep_code) -> ep_code
    | None -> ([%Michelson ({| { FAILWITH } |} : string -> bytes)] "ENTRYPOINT_NOT_FOUND" : bytes)
  in
  match ((Bytes.unpack packed_ep) : (bytes * full_storage -> return) option) with
  | Some lambda -> lambda (packed_param, (store, config))
  | None -> ([%Michelson ({| { FAILWITH } |} : string -> return)] "UNPACKING_FAILED" : return)
