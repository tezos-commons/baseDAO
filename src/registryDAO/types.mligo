// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"

// k, v parameters of the registry contract.
type registry_key = string
type registry_value = string

type registry = (registry_key, registry_value) map
type registry_affected = (registry_key, proposal_key) map
type registry_diff = (registry_key * registry_value option) list
type proposal_receivers = address set

type update_receiver_param =
  | Add_receivers of (address list)
  | Remove_receivers of (address list)

// For reference only
// ```
// type proposal_metadata_transfers =
//   { agoraPostID : nat
//   ; transfers : transfer_type list
//   }
//
// type proposal_metadata_updates =
//   { agoraPostID : nat
//   ; updates : registry_diff
//   }
//
// type proposal_metadata_update_receivers =
//   { update_receivers : update_receiver_param
//   }
//
// type proposal_metadata =
//   | Normal_metadata of proposal_metadata_updates
//   | Update_receivers_metadata of update_receiver_param
//   | Transfers_metadata of proposal_metadata_transfers
//   | Configuration_metadata
// ```

type proposal_type =
  | Normal_proposal of registry_diff
  | Update_receivers_proposal of update_receiver_param
  | Configuration_proposal
  | Transfer_proposal of transfer_type list

type lookup_registry_param =
  [@layout:comb]
  { key : registry_key
  ; callback : address
  }

type lookup_registry_view = (registry_key * (registry_value option)) contract



// -- Unpack Helpers (fail if the unpacked result is none) -- //

let unpack_registry (key_name, packed_b: string * bytes) : registry =
  match ((Bytes.unpack packed_b) : (registry option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> registry)]
              ("UNPACKING_FAILED", key_name) : registry)

let unpack_registry_affected (key_name, packed_b: string * bytes) : registry_affected =
  match ((Bytes.unpack packed_b) : (registry_affected option)) with
  |  Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> registry_affected)]
              ("UNPACKING_FAILED", key_name) : registry_affected)

let unpack_proposal_receivers (key_name, packed_b: string * bytes) : proposal_receivers =
  match ((Bytes.unpack packed_b) : (proposal_receivers option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> proposal_receivers)]
              ("UNPACKING_FAILED", key_name) : proposal_receivers)

let unpack_lookup_registry_param (key_name, packed_b: string * bytes) : lookup_registry_param =
  match ((Bytes.unpack packed_b) : (lookup_registry_param option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> lookup_registry_param)]
              ("UNPACKING_FAILED", key_name) : lookup_registry_param)
