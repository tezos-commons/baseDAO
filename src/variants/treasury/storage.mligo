// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

type custom_ep_param = unit

type contract_extra =
  { frozen_scale_value : nat
  ; frozen_extra_value : nat
  ; max_proposal_size : nat
  ; slash_scale_value : nat
  ; slash_division_value : nat
  ; min_xtz_amount : tez
  ; max_xtz_amount : tez
  }

let default_extra : contract_extra =
  { frozen_scale_value = 1n
  ; frozen_extra_value = 0n
  ; max_proposal_size = 1000n
  ; slash_scale_value = 1n
  ; slash_division_value = 1n
  ; min_xtz_amount = 0tz
  ; max_xtz_amount = 1tz
  }

#endif
