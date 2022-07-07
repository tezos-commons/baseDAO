// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT
#define VARIANT

#include "error_codes.mligo"
#include "helper/unpack.mligo"
#include "variants/lambda/types.mligo"
#include "types.mligo"

// -----------------------------------------------------------------
// Proposal check
//
// This should implement the proposal_check logic of the variant and should
// verify whether a proposal can be submitted.  It checks 2 things: the
// proposal itself and the amount of tokens frozen upon submission.  It allows
// the DAO to reject a proposal by arbitrary logic and captures bond
// requirements
// -----------------------------------------------------------------

let handler_exists(handler_name, ce : string * contract_extra) : bool =
  Big_map.mem handler_name ce.lambdas

let unpack_proposal_metadata (pm: proposal_metadata) : lambda_dao_proposal_metadata =
  match ((Bytes.unpack pm) : (lambda_dao_proposal_metadata option)) with
  | Some (v) -> v
  | None -> (failwith unpacking_proposal_metadata_failed : lambda_dao_proposal_metadata)

let proposal_check (propose_params, ce : propose_params * contract_extra) : unit =
  match unpack_proposal_metadata(propose_params.proposal_metadata) with
    | Add_handler ahp -> if handler_exists(ahp.name, ce) then (failwith proposal_handler_exists : unit) else unit
    | Remove_handler rhp -> if handler_exists(rhp, ce) then unit else (failwith proposal_handler_not_found : unit)
    | Execute_handler ehp -> (match Big_map.find_opt ehp.handler_name ce.lambdas with
          | Some hinfo ->
              let
                hc_input = (ehp.packed_argument, ce.handler_storage)
              in hinfo.handler_check hc_input
          | None -> (failwith proposal_handler_not_found: unit))

let add_handler(ce, ahp : contract_extra * add_handler_param) : contract_extra =
  if handler_exists(ahp.name, ce)
    then
     (failwith proposal_handler_exists: contract_extra)
    else let
      handler_info = { code = ahp.code ; handler_check = ahp.handler_check; is_active = true }
      in {ce with lambdas = Big_map.add ahp.name handler_info ce.lambdas }

let remove_handler(ce, rhp : contract_extra * remove_handler_param) : contract_extra = {ce with lambdas = Big_map.remove rhp ce.lambdas }

let execute_handler(proposer, frozen, ce, md, ehp : address * nat * contract_extra * proposal_metadata * execute_handler_param) : ((operation list) * contract_extra * (address option)) =
  match Big_map.find_opt ehp.handler_name ce.lambdas with
    | Some hinfo ->
        let
          ph_input =
            { packed_argument = ehp.packed_argument
            ; handler_storage = ce.handler_storage
            ; proposal_info = { from = proposer; frozen_token = frozen; proposal_metadata = md }
            } in
        let ph_out = hinfo.code ph_input
        in (ph_out.operations, { ce with handler_storage = ph_out.handler_storage }, ph_out.guardian)
    | None -> (failwith proposal_handler_not_found: ((operation list) * contract_extra * (address option)))

// -----------------------------------------------------------------
// decision_callback
//
// The decision callback is executed based on a successful proposal.  It has
// access to the proposal, can modify `contractExtra` and perform arbitrary
// operations.
// -----------------------------------------------------------------

let decision_callback (input : decision_callback_input)
    : decision_callback_output = let
  (ops, new_ce, new_guardian) = match unpack_proposal_metadata(input.proposal.metadata) with
    | Add_handler ahp -> (([] : operation list), add_handler(input.extras, ahp), None)
    | Remove_handler rhp -> (([] : operation list), remove_handler(input.extras, rhp), None)
    | Execute_handler ehp -> execute_handler(input.proposal.proposer, input.proposal.proposer_frozen_token, input.extras, input.proposal.metadata, ehp)
  in { operations = ops; extras = new_ce; guardian = new_guardian }

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

#endif
