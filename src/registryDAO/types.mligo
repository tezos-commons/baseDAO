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

type transfer_proposal =
  { agora_post_id : nat
  ; transfers : transfer_type list
  ; registry_diff : registry_diff
  }

type config_proposal =
  { frozen_scale_value : nat option
  ; frozen_extra_value : nat option
  ; slash_scale_value : nat option
  ; slash_division_value : nat option
  ; max_proposal_size : nat option
  }

// Registry dao `proposal_metadata` contains the type of proposal.
type registry_dao_proposal_metadata =
  | Update_receivers_proposal of update_receiver_param
  | Configuration_proposal of config_proposal
  | Transfer_proposal of transfer_proposal
  | Update_guardian of update_guardian

type lookup_registry_param =
  [@layout:comb]
  { key : registry_key
  ; callback : address
  }

type lookup_registry_view = (registry_key * (registry_value option)) contract

type initial_registryDAO_storage =
  { base_data : initial_data
  ; frozen_scale_value : nat
  ; frozen_extra_value : nat
  ; max_proposal_size : nat
  ; slash_scale_value : nat
  ; slash_division_value : nat
  ; min_xtz_amount : tez
  ; max_xtz_amount : tez
  }

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

let unpack_proposal_metadata (pm: proposal_metadata) : registry_dao_proposal_metadata =
  match ((Bytes.unpack pm) : (registry_dao_proposal_metadata option)) with
  | Some (v) -> v
  | None -> (failwith ("UNPACKING_PROPOSAL_METADATA_FAILED") : registry_dao_proposal_metadata)
