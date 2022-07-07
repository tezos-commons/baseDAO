// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "../../error_codes.mligo"

type proposal_metadata = bytes
type handler_storage = (string, bytes) map

type propose_params =
  [@layout:comb]
  { from : address
  ; frozen_token : nat
  ; proposal_metadata : bytes
  }

type ph_input =
  { packed_argument : bytes
  ; handler_storage : handler_storage
  ; proposal_info : propose_params
  }

type ph_output =
  { operations : operation list
  ; handler_storage : handler_storage
  ; guardian : address option
  }

type proposal_handler = ph_input -> ph_output
type handler_check = (bytes * handler_storage) -> unit

type handler_info =
  { code : proposal_handler
  ; handler_check : handler_check
  ; is_active : bool
  }

type lambdas = (string, handler_info) big_map

// This type should define the type of the contract-extra field in storage
// for this variant.
type lambda_contract_extra =
  { handler_storage : handler_storage
  ; lambdas : lambdas
  }

type add_handler_param =
    { name : string
    ; code : proposal_handler
    ; handler_check : ((bytes * handler_storage) -> unit)
    }

type remove_handler_param = string
type execute_handler_param =
  { handler_name: string
  ; packed_argument: bytes
  }

type lambda_dao_proposal_metadata =
  | Add_handler of add_handler_param
  | Remove_handler of remove_handler_param
  | Execute_handler of execute_handler_param

let unpack_proposal_metadata (pm: proposal_metadata) : lambda_dao_proposal_metadata =
  match ((Bytes.unpack pm) : (lambda_dao_proposal_metadata option)) with
  | Some (v) -> v
  | None -> (failwith unpacking_proposal_metadata_failed : lambda_dao_proposal_metadata)
