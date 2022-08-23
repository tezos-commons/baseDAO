// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT
#define VARIANT

#include "error_codes.mligo"
#include "helper/unpack.mligo"
#include "variants/lambda/types.mligo"
#include "variants/lambda/common.mligo"
#include "variants/lambdatreasury/types.mligo"
#include "types.mligo"
#include "defaults.mligo"

let check_token_locked_and_proposal_size (frozen_token, required_token_lock, proposal_size, max_proposal_size
  : nat * nat * nat * nat) : string option =
    if (frozen_token <> required_token_lock) then Some (wrong_token_amount_err_msg)
    else if (proposal_size >= max_proposal_size) then Some (large_proposal_err_msg)
    else None

let proposal_check (propose_params, ce : propose_params * contract_extra) : unit =

  let proposal_size = Bytes.length(propose_params.proposal_metadata) in
  let frozen_scale_value = fetch_nat("frozen_scale_value", ce.handler_storage) in
  let frozen_extra_value = fetch_nat("frozen_extra_value", ce.handler_storage) in
  let max_proposal_size = fetch_nat("max_proposal_size", ce.handler_storage) in

  let required_token_lock = frozen_scale_value * proposal_size + frozen_extra_value in

  let _ : unit =
    match check_token_locked_and_proposal_size(propose_params.frozen_token, required_token_lock, proposal_size, max_proposal_size) with
    | Some err_msg -> fail_proposal_check(err_msg)
    | None -> unit

  in common_proposal_check(propose_params, ce)

// -----------------------------------------------------------------
// decision_callback
//
// The decision callback is executed based on a successful proposal.  It has
// access to the proposal, can modify `contractExtra` and perform arbitrary
// operations.
// -----------------------------------------------------------------

let decision_callback (input : decision_callback_input)
    : decision_callback_output = common_decision_callback(input)

// -----------------------------------------------------------------
// Rejected proposal slash value is called when a proposal is rejected, and the
// value that voters get back can be slashed. This procedure should return the
// amount to be slashed.
// -----------------------------------------------------------------

let rejected_proposal_slash_value (_, _ : proposal * contract_extra) : nat = 0n

// -----------------------------------------------------------------
// Type that make up the custom entrypoint of the variant.
//
// -----------------------------------------------------------------
type custom_ep_param = unit

// -----------------------------------------------------------------
// Custom entrypoint handler for this variant
//
// -----------------------------------------------------------------
let custom_ep (_, storage : custom_ep_param * storage): operation list * storage
  = (([] : operation list), storage)

type contract_extra = unit

let default_lambda_treasury_DAO_storage (data : initial_treasuryDAO_storage) : storage =
  let store = default_storage (data.base_data) in
  let handler_storage = Map.literal
    [ ("frozen_extra_value", Bytes.pack data.frozen_scale_value)
    ; ("max_proposal_size", Bytes.pack data.max_proposal_size)
    ; ("slash_scale_value", Bytes.pack data.slash_scale_value)
    ; ("slash_division_value", Bytes.pack data.slash_division_value)
    ; ("min_xtz_amount", Bytes.pack data.min_xtz_amount)
    ; ("max_xtz_amount", Bytes.pack data.max_xtz_amount)
    ] in
  { store with
    extra =
      { default_extra with handler_storage = handler_storage
      }
  }
#endif
