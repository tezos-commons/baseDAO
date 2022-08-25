// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "../../types.mligo"
#include "../../error_codes.mligo"

type initial_treasuryDAO_storage =
  { base_data : initial_data
  ; frozen_scale_value : nat
  ; frozen_extra_value : nat
  ; max_proposal_size : nat
  ; slash_scale_value : nat
  ; slash_division_value : nat
  ; min_xtz_amount : tez
  ; max_xtz_amount : tez
  }
