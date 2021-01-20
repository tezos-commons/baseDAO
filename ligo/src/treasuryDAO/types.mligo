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
    Xtz_transfer_type of xtz_transfer
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
  ( nat // a : frozen_scale_value
  * nat // b : frozen_extra_value
  * nat // s_max : max_proposal_size
  * nat // c : slash_scale_value
  * nat // d : slash_division_value
  * tez // y : min_xtz_amount
  * tez // z : max_xtz_amount
  )
