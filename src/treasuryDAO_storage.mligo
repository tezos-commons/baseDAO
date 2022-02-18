// Instantiation-specific stored data

type contract_extra =
  { frozen_scale_value : nat option
  ; frozen_extra_value : nat option
  ; max_proposal_size : nat option
  ; slash_scale_value : nat option
  ; slash_division_value : nat option
  ; min_xtz_amount : tez option
  ; max_xtz_amount : tez option
  }

let default_extra : contract_extra =
  { frozen_scale_value = (None : nat option)
  ; frozen_extra_value = (None : nat option)
  ; max_proposal_size = (None : nat option)
  ; slash_scale_value = (None : nat option)
  ; slash_division_value = (None : nat option)
  ; min_xtz_amount = (None : tez option)
  ; max_xtz_amount = (None : tez option)
  }
