// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"

// k, v parameters of the registry contract.
type registry_key = string
type registry_value = string

type registry = (registry_key, registry_value) map
type registry_affected = (registry_key, proposal_key) map
type registry_diff = (registry_key * registry_value option) list
