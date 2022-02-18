type registry_key = string
type registry_value = string
type registry = (registry_key, registry_value) map
type proposal_key = bytes

type registry = (registry_key, registry_value) map
type contract_extra =
  { registry : registry
  ; registry_affected : (registry_key, proposal_key) map
  ; proposal_receivers : address set
  ; frozen_scale_value : nat option
  ; frozen_extra_value : nat option
  ; max_proposal_size : nat option
  ; slash_scale_value : nat option
  ; slash_division_value : nat option
  ; min_xtz_amount : tez option
  ; max_xtz_amount : tez option
  }

let default_extra : contract_extra =
  { registry = (Map.empty : registry)
  ; registry_affected = (Map.empty : (registry_key, proposal_key) map)
  ; proposal_receivers = (Set.empty : address set)
  ; frozen_scale_value = (None : nat option)
  ; frozen_extra_value = (None : nat option)
  ; max_proposal_size = (None : nat option)
  ; slash_scale_value = (None : nat option)
  ; slash_division_value = (None : nat option)
  ; min_xtz_amount = (None : tez option)
  ; max_xtz_amount = (None : tez option)
  }
