// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"


(*
 * Contract Extra
 *)

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

// Treasury dao `proposal_metadata` contains the type of proposal.
// Currently only `transfer_proposal` type exists.
type treasury_dao_proposal_metadata = transfer_proposal


// -- Unpack Helpers (fail if the unpacked result is none) -- //

let unpack_proposal_metadata (pm: proposal_metadata) : treasury_dao_proposal_metadata =
  match ((Bytes.unpack pm) : (treasury_dao_proposal_metadata option)) with
  | Some (v) -> v
  | None -> (failwith ("UNPACKING_PROPOSAL_METADATA_FAILED") : treasury_dao_proposal_metadata)
