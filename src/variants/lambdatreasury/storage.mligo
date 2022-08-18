// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

#include "variants/lambda/types.mligo"
#include "error_codes.mligo"

// Xtz transfer amount cannot be 0
let zero_mutez_err_msg = "ZERO_MUTEZ"

let too_small_xtz_err_msg = "LOW_XTZ"

// Xtz transfer amount cannot be bigger than `max_xtz_amount`
let too_large_xtz_err_msg = "HIGH_XTZ"

let fail_proposal_check_ (msg : string) : unit =
  ([%Michelson ({| { FAILWITH } |} : (nat * string) -> unit)]
    (fail_proposal_check, msg) : unit)

type config_proposal =
  { frozen_scale_value : nat option
  ; frozen_extra_value : nat option
  ; slash_scale_value : nat option
  ; slash_division_value : nat option
  ; max_proposal_size : nat option
  }

type xtz_transfer =
  [@layout:comb]
  { amount : tez
  ; recipient : address
  }

type token_id = nat
// FA2 transfer types
type transfer_destination =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }
type transfer_item =
  [@layout:comb]
  { from_ : address
  ; txs : transfer_destination list
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

type transfer_proposal =
  { agora_post_id : nat
  ; transfers : transfer_type list
  }

// This type should define the type of the contract-extra field in storage
// for this variant.
type contract_extra =
  { handler_storage : handler_storage
  ; lambdas : lambdas
  }

type transfer_params = transfer_item list

let handle_transfer (ops, transfer_type : (operation list) * transfer_type) : (operation list) =
  match transfer_type with
  | Token_transfer_type tt ->
    begin
    match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
          : transfer_params contract option) with
    | Some contract ->
        (Tezos.transaction tt.transfer_list 0mutez contract) :: ops
    | None -> (failwith fail_decision_callback : operation list)
    end
  | Xtz_transfer_type xt ->
    begin
    match (Tezos.get_contract_opt xt.recipient : unit contract option) with
    | Some contract ->
        (Tezos.transaction unit xt.amount contract) :: ops
    | None -> (failwith fail_decision_callback : operation list)
    end


// Uncomment the following lines and use them to implement proposal handlers
// and proposal check functions, and then use the `lambda_map` defined further
// down to include them in the storage.
let transfer_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : transfer_proposal option) with
    | Some tp ->
      // handle transfers
      let transfers = tp.transfers in
      let ops = List.fold handle_transfer transfers ([] : operation list) in
      { operations = ops; handler_storage = ph_input.handler_storage; guardian = None }
    | None -> failwith ""

let fetch_tez(key, hs : string * handler_storage) : tez =
  match (Map.find_opt key hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : tez option) with
        | Some v -> v
        | _ -> failwith "")
    | _ -> failwith ""

let fetch_nat(key, hs : string * handler_storage) : nat =
  match (Map.find_opt key hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : nat option) with
        | Some v -> v
        | _ -> failwith "")
    | _ -> failwith ""

let check_xtz_transfer (xt, min_xtz_amount, max_xtz_amount : xtz_transfer * tez * tez) : string option =
  if (xt.amount = 0mutez) then Some zero_mutez_err_msg
  else if (xt.amount < min_xtz_amount) then Some too_small_xtz_err_msg
  else if (xt.amount > max_xtz_amount) then Some too_large_xtz_err_msg
  else None

let transfer_proposal_handler_check (ptp, hs : bytes * handler_storage) : unit =
  match (Bytes.unpack ptp : transfer_proposal option) with
    | Some tp ->
      let min_xtz_amount = fetch_tez("min_xtz_amount", hs) in
      let max_xtz_amount = fetch_tez("max_xtz_amount", hs) in
      let is_all_transfers_valid (transfer_type: transfer_type) =
        match transfer_type with
        | Token_transfer_type _tt -> unit
        | Xtz_transfer_type xt ->
            begin
              match check_xtz_transfer(xt, min_xtz_amount, max_xtz_amount) with
              | Some err_msg -> fail_proposal_check_(err_msg)
              | None -> unit
            end
      in
        List.iter is_all_transfers_valid tp.transfers
    | None -> failwith ""

let set_hs_nat(key, val, hs : string * nat * handler_storage) : handler_storage =
  Map.update key (Some (Bytes.pack val)) hs

let update_guardian_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : address option) with
    | Some guardian -> { operations = []; handler_storage = ph_input.handler_storage; guardian = Some guardian }
    | None -> failwith ""

let update_guardian_handler_check (_, _ : bytes * handler_storage) : unit = unit

let update_contract_delegate_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : (key_hash option) option) with
    | Some mdelegate -> { operations = [Tezos.set_delegate mdelegate]; handler_storage = ph_input.handler_storage; guardian = None }
    | None -> failwith ""

let update_contract_delegate_handler_check (_, _ : bytes * handler_storage) : unit = unit

let lambda_map : lambdas = Big_map.literal
  [ ("transfer_proposal", { code = transfer_proposal_handler; handler_check = transfer_proposal_handler_check; is_active = true })
  ; ("update_guardian_proposal", { code = update_guardian_proposal_handler; handler_check = update_guardian_handler_check; is_active = true })
  ; ("update_contract_delegate_proposal", { code = update_contract_delegate_proposal_handler; handler_check = update_contract_delegate_handler_check; is_active = true })
  ]

// Use the following sample to define each handler instance, and replace the empty list
// above with it.
//   [("handler_1",
//       { code = proposal_handler_1
//       ; handler_check = handler_check_1
//       ; is_active = true
//       }
//    )
//   ]

// This should define a default value for the contract-extra field for the
// variant.
let default_extra : contract_extra =
  { handler_storage = (Map.empty : handler_storage)
  ; lambdas = lambda_map
  }

#endif
