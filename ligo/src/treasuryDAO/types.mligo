// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"

(*
 * Proposal Metadata
 *)

type xtz_transfer =
  [@layout:comb]
  { amount : tez
  ; recipient : address
  }

type token_transfer =
  [@layout:comb]
  { contract_address : address
  ; transfer_list : transfer_item list
  }

type transfer_type =
  [@layout:comb]
  | Xtz_transfer_type of xtz_transfer
  | Token_transfer_type of token_transfer


// For reference only
// ```
// type proposal_metadata =
//   [@layout:comb]
//   { agora_post_id : nat
//   ; transfers : transfer_type list
//   }
// ```

(*
 * Contract Extra
 *)

type treasury_contract_extra =
  ( nat // frozen_scale_value
  * nat // frozen_extra_value
  * nat // max_proposal_size
  * nat // slash_scale_value
  * nat // slash_division_value
  * tez // min_xtz_amount
  * tez // max_xtz_amount
  )
