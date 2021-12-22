// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !VARIANT
#define VARIANT

#include "types.mligo"
#include "common/errors.mligo"

let proposal_check (pp, ce : propose_params * contract_extra) : unit =
  let packed_ep =
    match Map.find_opt "proposal_check" ce with
    | Some (ep_code) -> ep_code
    | None -> ([%Michelson ({| { FAILWITH } |} : string -> bytes)] "ENTRYPOINT_NOT_FOUND1" : bytes)
  in
  match ((Bytes.unpack packed_ep) : (propose_params * contract_extra -> unit) option) with
  | Some lambda -> lambda (pp, ce)
  | None -> ([%Michelson ({| { FAILWITH } |} : string -> unit)] "UNPACKING_FAILED1" : unit)

let decision_lambda (input : decision_lambda_input)
    : decision_lambda_output =
  let packed_ep =
    match Map.find_opt "decision_lambda" input.extras with
    | Some (ep_code) -> ep_code
    | None -> ([%Michelson ({| { FAILWITH } |} : string -> bytes)] "ENTRYPOINT_NOT_FOUND2" : bytes)
  in
  match ((Bytes.unpack packed_ep) : decision_lambda option) with
  | Some lambda -> lambda (input)
  | None -> ([%Michelson ({| { FAILWITH } |} : string -> decision_lambda_output)] "UNPACKING_FAILED2" : decision_lambda_output)

let rejected_proposal_slash_value (p, ce : proposal * contract_extra) : nat =
  let packed_ep =
    match Map.find_opt "rejected_proposal_slash_value" ce with
    | Some (ep_code) -> ep_code
    | None -> ([%Michelson ({| { FAILWITH } |} : string -> bytes)] "ENTRYPOINT_NOT_FOUND3" : bytes)
  in
  match ((Bytes.unpack packed_ep) : (proposal * contract_extra -> nat) option) with
  | Some lambda -> lambda (p, ce)
  | None -> ([%Michelson ({| { FAILWITH } |} : string -> nat)] "UNPACKING_FAILED3" : nat)

type custom_ep_param = unit
let custom_ep (_, storage, _ : custom_ep_param * storage * config): operation list * storage
  = (([] : operation list), storage)
#endif
