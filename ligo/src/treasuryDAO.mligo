// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "defaults.mligo"
#include "types.mligo"
#include "proposal.mligo"
#include "helper/unpack.mligo"
#include "base_DAO.mligo"

#include "treasuryDAO/types.mligo"

// -------------------------------------
// Helpers
// -------------------------------------

let unpack_transfer_type_list (key_name, p : string * ((string, bytes) map)) : transfer_type list =
  let b = unpack_key (key_name, p)
  in match ((Bytes.unpack b) : ((transfer_type list) option)) with
    Some (v) -> v
  | None -> (failwith "UNPACKING_NOT_TRANSFER_TYPE_LIST" : transfer_type list)

// -------------------------------------
// Configuration Lambdas
// -------------------------------------

let treasury_DAO_proposal_check (params, store : propose_params * storage) : bool =
  let proposal_size = Bytes.size(Bytes.pack(params.proposal_metadata)) in
  let frozen_scale_value = unpack_nat("a", store.extra) in
  let frozen_extra_value = unpack_nat("b", store.extra) in
  let max_proposal_size = unpack_nat("s_max", store.extra) in
  let min_xtz_amount = unpack_tez("y", store.extra) in
  let max_xtz_amount = unpack_tez("z", store.extra) in

  let requred_token_lock = frozen_scale_value * proposal_size + frozen_extra_value in

  if (params.frozen_token = requred_token_lock) && (proposal_size < max_proposal_size) then
    let ts = unpack_transfer_type_list("transfers", params.proposal_metadata) in
    let is_all_transfers_valid (is_valid, transfer_type: bool * transfer_type) =
      if is_valid then
        match transfer_type with
            Token_transfer_type tt -> is_valid
          | Xtz_transfer_type xt ->
              if (min_xtz_amount <= xt.amount && xt.amount <= max_xtz_amount) then
                is_valid
              else
                false
      else
        false
    in List.fold is_all_transfers_valid ts true
  else
    false


let treasury_DAO_rejected_proposal_return_value (params, store : proposal * storage) : nat =
  let slash_scale_value = unpack_nat("c", store.extra) in
  let slash_division_value =  unpack_nat("d", store.extra)
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value


let treasury_DAO_decision_lambda (proposal, store : proposal * storage) : return =
  let propose_param : propose_params = {
    frozen_token = proposal.proposer_frozen_token;
    proposal_metadata = proposal.metadata
    } in
  let proposal_key = to_proposal_key (propose_param, proposal.proposer) in
  let ts = unpack_transfer_type_list("transfers", proposal.metadata) in
  let handle_transfer (acc, transfer_type : (bool * storage * operation list) * transfer_type) =
      let (is_valid, store, ops) = acc in
      if is_valid then
        match transfer_type with
          Token_transfer_type tt ->
            let result = match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
                : transfer_params contract option) with
              Some contract ->
                let token_transfer_operation = Tezos.transaction tt.transfer_list 0mutez contract
                in (is_valid, store, token_transfer_operation :: ops)
            | None ->
                (false, store, ops)
            in result
        | Xtz_transfer_type xt ->
            let result = match (Tezos.get_contract_opt xt.recipient
                : unit contract option) with
              Some contract ->
                let xtz_transfer_operation = Tezos.transaction unit xt.amount contract
                in (is_valid, store, xtz_transfer_operation :: ops)
            | None ->
                (false, store, ops)
            in result
      else
        (false, store, ops)
  in
  let (is_valid, store, ops) = List.fold handle_transfer ts (true, store, ([] : operation list)) in
  if is_valid then
    (ops, store)
  else
    // TODO: [#87] Improve handling of failed proposals
    (failwith("FAIL_DECISION_LAMBDA"): return)

// A custom entrypoint needed to receive xtz, since most `basedao` entrypoints
// prohibit non-zero xtz transfer.
let receive_xtz_entrypoint (params, full_storage : bytes * full_storage): return_with_full_storage =
  (([]: operation list), full_storage)

// -------------------------------------
// Storage Generator
// -------------------------------------

let default_treasury_DAO_full_storage (admin, token_address, contract_extra
    : (address * address * treasury_contract_extra)) : full_storage =
  let fs = default_full_storage (admin, token_address) in
  let (a, b, s_max, c, d, y, z) = contract_extra in
  let new_storage = { fs.0 with
    extra = Map.literal [
          ("a" , Bytes.pack a); // frozen_scale_value
          ("b" , Bytes.pack b); // frozen_extra_value
          ("s_max" , Bytes.pack s_max); // max_proposal_size
          ("c" , Bytes.pack c); // slash_scale_value
          ("d" , Bytes.pack d); // slash_division_value
          ("y" , Bytes.pack y); // min_xtz_amount
          ("z" , Bytes.pack z); // max_xtz_amount
          ];
  } in
  let new_config = { fs.1 with
    proposal_check = treasury_DAO_proposal_check;
    rejected_proposal_return_value = treasury_DAO_rejected_proposal_return_value;
    decision_lambda = treasury_DAO_decision_lambda;
    custom_entrypoints = Map.literal ["receive_xtz", Bytes.pack (receive_xtz_entrypoint)]
    } in
  (new_storage, new_config)
