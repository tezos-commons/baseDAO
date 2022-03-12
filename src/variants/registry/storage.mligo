// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

type registry_key = string
type registry_value = string
type registry = (registry_key, registry_value) map
type proposal_key = bytes

type registry = (registry_key, registry_value) map
type contract_extra =
  { registry : registry
  ; registry_affected : (registry_key, proposal_key) map
  ; proposal_receivers : address set
  ; frozen_scale_value : nat
  ; frozen_extra_value : nat
  ; max_proposal_size : nat
  ; slash_scale_value : nat
  ; slash_division_value : nat
  ; min_xtz_amount : tez
  ; max_xtz_amount : tez
  }

let default_extra : contract_extra =
  { registry = (Map.empty : registry)
  ; registry_affected = (Map.empty : (registry_key, proposal_key) map)
  ; proposal_receivers = (Set.empty : address set)
  ; frozen_scale_value = 1n
  ; frozen_extra_value = 0n
  ; max_proposal_size = 1000n
  ; slash_scale_value = 1n
  ; slash_division_value = 1n
  ; min_xtz_amount = 0tez
  ; max_xtz_amount = 1tez
  }

#endif
