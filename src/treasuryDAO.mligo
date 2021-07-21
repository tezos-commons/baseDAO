// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "common/errors.mligo"
#include "common/types.mligo"
#include "defaults.mligo"
#include "types.mligo"
#include "proposal.mligo"
#include "helper/unpack.mligo"
#include "base_DAO.mligo"

#include "treasuryDAO/types.mligo"

// -------------------------------------
// Configuration Lambdas
// -------------------------------------

let treasury_DAO_proposal_check (params, extras : propose_params * contract_extra) : unit =
  let proposal_size = Bytes.size(params.proposal_metadata) in
  let frozen_scale_value = unpack_nat(find_big_map("frozen_scale_value", extras)) in
  let frozen_extra_value = unpack_nat(find_big_map("frozen_extra_value", extras)) in
  let max_proposal_size = unpack_nat(find_big_map("max_proposal_size", extras)) in
  let min_xtz_amount = unpack_tez(find_big_map("min_xtz_amount", extras)) in
  let max_xtz_amount = unpack_tez(find_big_map("max_xtz_amount", extras)) in

  let required_token_lock = frozen_scale_value * proposal_size + frozen_extra_value in

  let _ : unit =
    match check_token_locked_and_proposal_size(params.frozen_token, required_token_lock, proposal_size, max_proposal_size) with
    | Some err_msg -> fail_proposal_check(err_msg)
    | None -> unit in

  match unpack_proposal_metadata(params.proposal_metadata) with
    | Update_guardian addr -> unit
    | Transfer_proposal tpm ->
        let is_all_transfers_valid (transfer_type: transfer_type) =
          match transfer_type with
          | Token_transfer_type _tt -> unit
          | Xtz_transfer_type xt ->
              begin
                match check_xtz_transfer(xt, min_xtz_amount, max_xtz_amount) with
                | Some err_msg -> fail_proposal_check(err_msg)
                | None -> unit
              end
        in
          List.iter is_all_transfers_valid tpm.transfers

let treasury_DAO_rejected_proposal_slash_value (params, extras : proposal * contract_extra) : nat =
  let slash_scale_value = unpack_nat(find_big_map("slash_scale_value", extras)) in
  let slash_division_value =  unpack_nat(find_big_map("slash_division_value", extras))
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value

let handle_transfer (ops, transfer_type : (operation list) * transfer_type) : (operation list) =
  match transfer_type with
  | Token_transfer_type tt ->
    begin
      match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
          : transfer_params contract option) with
      | Some contract ->
          (Tezos.transaction tt.transfer_list 0mutez contract) :: ops
      | None -> (failwith("FAIL_DECISION_LAMBDA") : operation list)
    end
  | Xtz_transfer_type xt ->
    begin
      match (Tezos.get_contract_opt xt.recipient : unit contract option) with
      | Some contract ->
          (Tezos.transaction unit xt.amount contract) :: ops
      | None -> (failwith("FAIL_DECISION_LAMBDA") : operation list)
    end

let treasury_DAO_decision_lambda (input : decision_lambda_input)
    : decision_lambda_output =
  let (proposal, extras) = (input.proposal, input.extras) in
  let ops = ([] : operation list) in

  match unpack_proposal_metadata(proposal.metadata) with
    | Transfer_proposal (tpm) -> let
        ops = List.fold handle_transfer tpm.transfers ops in
        { operations = ops; extras = extras; guardian = (None : (address option)) }
    | Update_guardian guardian ->
        { operations = ops; extras = extras; guardian = Some(guardian) }

// A custom entrypoint needed to receive xtz, since most `basedao` entrypoints
// prohibit non-zero xtz transfer.
let receive_xtz_entrypoint (_params, full_store : bytes * full_storage) : return =
  (nil_op, full_store.0)

// -------------------------------------
// Storage Generator
// -------------------------------------

let default_treasury_DAO_full_storage (data : initial_treasuryDAO_storage) : full_storage =
  let (store, config) = default_full_storage (data.base_data) in
  let new_storage = { store with
    extra = Big_map.literal [
      ("frozen_scale_value" , Bytes.pack data.frozen_scale_value);
      ("frozen_extra_value" , Bytes.pack data.frozen_extra_value);
      ("max_proposal_size" , Bytes.pack data.max_proposal_size);
      ("slash_scale_value" , Bytes.pack data.slash_scale_value);
      ("slash_division_value" , Bytes.pack data.slash_division_value);
      ("min_xtz_amount" , Bytes.pack data.min_xtz_amount);
      ("max_xtz_amount" , Bytes.pack data.max_xtz_amount);
      ];
  } in
  let new_config = { config with
    proposal_check = treasury_DAO_proposal_check;
    rejected_proposal_slash_value = treasury_DAO_rejected_proposal_slash_value;
    decision_lambda = treasury_DAO_decision_lambda;
    custom_entrypoints = Big_map.literal [("receive_xtz", Bytes.pack (receive_xtz_entrypoint))];
    }
  in (new_storage, new_config)
