// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT_COMMON
#define VARIANT_COMMON

#include "../../error_codes.mligo"
#include "../../helper/unpack.mligo"
#include "types.mligo"
#include "../../types.mligo"

// Proposal handler was not found
let proposal_handler_not_found = "PROPOSAL_HANDLER_NOT_FOUND"
let proposal_handler_exists = "PROPOSAL_HANDLER_EXISTS"

let fail_proposal_check_ (msg : string) : unit =
  ([%Michelson ({| { FAILWITH } |} : (nat * string) -> unit)]
    (fail_proposal_check, msg) : unit)

// Checks if the handler exists in storage
let handler_exists(handler_name, ce : string * contract_extra) : bool =
  Big_map.mem handler_name ce.lambdas

// Checks if the handler exists in storage and is active
let handler_exists_and_active(handler_name, ce : string * contract_extra) : bool =
  match Big_map.find_opt handler_name ce.lambdas with
    | Some (v) -> v.is_active
    | None -> false

// The proposal check for lambdaDAO.
// 1. Add_handler proposal gets checked to ensure that no proposal handler with the input name exists already.
// 2. Remove handler proposal gets checked to ensure that an active proposal handler with the given name exist.
// 3. Execute_handler proposals gets checked to ensure that an active proposal handler with the given name exists, and
//    then calls that handler's "handler_check" procedure.
let common_proposal_check (propose_params, ce : propose_params * contract_extra) : unit =
  match unpack_proposal_metadata(propose_params.proposal_metadata) with
    | Add_handler ahp -> if handler_exists(ahp.name, ce) then (fail_proposal_check_(proposal_handler_exists) : unit) else unit
    | Remove_handler rhp -> if handler_exists_and_active(rhp, ce) then unit else (fail_proposal_check_(proposal_handler_not_found) : unit)
    | Execute_handler ehp -> (match Big_map.find_opt ehp.handler_name ce.lambdas with
          // Check if the handler is active, and if it is, fetch its handler-check lambda
          // and execute it with the input from this proposal.
          | Some hinfo ->
              if hinfo.is_active
                then let
                  hc_input = (ehp.packed_argument, ce.handler_storage)
                  in hinfo.handler_check hc_input
                else (fail_proposal_check_(proposal_handler_not_found): unit)
          | None -> (fail_proposal_check_(proposal_handler_not_found): unit))

let add_handler(ce, ahp : contract_extra * add_handler_param) : contract_extra =
  if handler_exists(ahp.name, ce)
    then
     (failwith proposal_handler_exists: contract_extra)
    else let
      // Insert the new handler and mark it active.
      handler_info = { code = ahp.code ; handler_check = ahp.handler_check; is_active = true }
      in {ce with lambdas = Big_map.add ahp.name handler_info ce.lambdas }

let disable_handler(ce, rhp : contract_extra * remove_handler_param) : contract_extra =
  match Big_map.find_opt rhp ce.lambdas with
    // If the handler exists, then mark it as inactive.
    | Some hinfo -> {ce with lambdas = Big_map.update rhp (Some {hinfo with is_active = false}) ce.lambdas }
    // If it does not, do nothing.
    | None -> ce

let execute_handler(proposer, frozen, ce, md, ehp : address * nat * contract_extra * proposal_metadata * execute_handler_param) : ((operation list) * contract_extra * (address option)) =
  match Big_map.find_opt ehp.handler_name ce.lambdas with
    | Some hinfo ->
        // If handler is present, execute it. Note that we don't check for active status of the
        // handler since we want inactive handlers to be executable as well, so that existing proposals
        // that use the handler can proceed to completion. After execution, store the updated
        // handler_storage, and return the emitted operations.
        let
          ph_input =
            { packed_argument = ehp.packed_argument
            ; handler_storage = ce.handler_storage
            ; proposal_info = { from = proposer; frozen_token = frozen; proposal_metadata = md }
            } in
        let ph_out = hinfo.code ph_input
        in (ph_out.operations, { ce with handler_storage = ph_out.handler_storage }, ph_out.guardian)
    | None -> (failwith bad_state: ((operation list) * contract_extra * (address option)))

let common_decision_callback (input : decision_callback_input)
    : decision_callback_output = let
  (ops, new_ce, new_guardian) = match unpack_proposal_metadata(input.proposal.metadata) with
    | Add_handler ahp -> (([] : operation list), add_handler(input.extras, ahp), None)
    | Remove_handler rhp -> (([] : operation list), disable_handler(input.extras, rhp), None)
    | Execute_handler ehp -> execute_handler(input.proposal.proposer, input.proposal.proposer_frozen_token, input.extras, input.proposal.metadata, ehp)
  in { operations = ops; extras = new_ce; guardian = new_guardian }

// Incorrect token amounts locked
let wrong_token_amount_err_msg = "WRONG_TOKEN_AMOUNT"

// Proposal size is bigger than `max_proposal_size`
let large_proposal_err_msg = "LARGE_PROPOSAL"

let fail_proposal_check (msg : string) : unit =
  ([%Michelson ({| { FAILWITH } |} : (nat * string) -> unit)]
    (fail_proposal_check, msg) : unit)

#endif
