// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT
#define VARIANT

#include "error_codes.mligo"
#include "helper/unpack.mligo"
#include "variants/lambda/types.mligo"
#include "variants/lambda/common.mligo"
#include "variants/lambdaregistry/storage.mligo"
#include "types.mligo"

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

let rejected_proposal_slash_value (params, ce : proposal * contract_extra) : nat =
  let slash_scale_value = fetch_nat("slash_scale_value", ce.handler_storage) in
  let slash_division_value = fetch_nat("slash_division_value", ce.handler_storage)
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value

// -----------------------------------------------------------------
// Type that make up the custom entrypoint of the variant.
//
// -----------------------------------------------------------------

type lookup_registry_param =
  [@layout:comb]
  { key : registry_key
  ; callback : address
  }

type custom_ep_param =
  | Lookup_registry of lookup_registry_param
  | RegistryCepDummy of unit

type lookup_registry_view = (registry_key * (registry_value option)) contract

// A custom entrypoint to fetch values from Registry
let lookup_registry (param, store : lookup_registry_param * storage) : operation list * storage =
  let view_contract : lookup_registry_view =
      match (Tezos.get_contract_opt(param.callback) : lookup_registry_view option) with
      | Some callback_contract -> callback_contract
      | None -> (failwith bad_view_contract: lookup_registry_view) in
  let contract_extra = store.extra in
  let registry : registry = fetch_registry(contract_extra.handler_storage) in
  let value_at_key : registry_value option = Map.find_opt param.key registry in
  let operation : operation = Tezos.transaction (param.key, value_at_key) 0mutez view_contract
  in ((operation :: nil_op), store)

// -----------------------------------------------------------------
// Custom entrypoint handler for this variant
//
// -----------------------------------------------------------------
let custom_ep (custom_ep_param, storage : custom_ep_param * storage): operation list * storage
  = match custom_ep_param with
  | Lookup_registry p -> lookup_registry(p, storage)
  | _ -> (([]: operation list), storage)

type contract_extra = unit
#endif
