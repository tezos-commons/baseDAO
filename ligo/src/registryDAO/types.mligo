// SPDX-FileCopyrightText: 2020 TQ Tezos
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

type proposal_type =
  | Normal_proposal of registry_diff
  | Update_receivers_proposal of update_receiver_param
  | Configuration_proposal

type lookup_registry_param =
  [@layout:comb]
  { key : registry_key
  ; callback : address
  }

type lookup_registry_view = (registry_key * (registry_value option)) contract
