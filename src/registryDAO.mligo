// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// The following include is required so that we have a function with an
// entrypoint type to use with `compile-storage` command to make Michelson
// storage expression.
#include "base_DAO.mligo"

#include "common/types.mligo"
#include "defaults.mligo"
#include "types.mligo"
#include "proposal.mligo"
#include "helper/unpack.mligo"

#include "registryDAO/types.mligo"

let apply_diff_registry (diff, registry : registry_diff * registry) : registry =
  let
    foldFn (registry, update: registry * (registry_key * registry_value option)) : registry =
      let registry_key = update.0 in
      let registry_value = update.1 in
      Map.update registry_key registry_value registry
  in List.fold foldFn diff registry

let apply_diff_registry_affected (proposal_key, diff, registry_affected : proposal_key
        * registry_diff * registry_affected) : registry_affected =
  let
    foldFn (registry_affected, update: registry_affected * (registry_key *
      registry_value option)) : registry_affected =
      let registry_key = update.0 in
      Map.update registry_key (Some (proposal_key)) registry_affected
  in List.fold foldFn diff registry_affected

let apply_diff (diff, ce : registry_diff * contract_extra) : contract_extra =
  let registry = unpack_registry(find_big_map("registry", ce)) in
  let new_registry : registry = apply_diff_registry (diff, registry) in
  Map.update "registry" (Some (Bytes.pack new_registry)) ce

let apply_diff_affected (proposal_key, diff, ce : proposal_key *
    registry_diff * contract_extra) : contract_extra =
  let registry_af = unpack_registry_affected(find_big_map("registry_affected", ce)) in
  let new_registry_af : registry_affected = apply_diff_registry_affected (proposal_key, diff, registry_af)
  in Map.update "registry_affected" (Some (Bytes.pack new_registry_af)) ce

(*
 * Proposal check lambda : returns true if following checks are true.
 * 1. if proposer has locked frozen_scale_value * s + frozen_extra_value
 * where s = size(pack(proposalMetadata)) and frozen_scale_value and frozen_extra_value
 * are from storage configuration.
 * 2. proposal size < max_proposal_size where max_proposal_size is from stored configuration.
 * 3. if transfers is present in the metadata, then it contains transfers such that each transfer:
 *   3.a. is a xtz transfer satisfying min_xtz_amount <= amount <= max_xtz_amount
 *   3.b. is a token transfer
 * where min_xtz_amount and max_xtz_amount are from storage configuration.
 *)

#if BYO_DAO
let registry_DAO_proposal_check (params, extras : propose_params * contract_extra) : bool =
  let proposal_size = Bytes.size(params.proposal_metadata) in
  let frozen_scale_value = unpack_nat(find_big_map("frozen_scale_value", extras)) in
  let frozen_extra_value = unpack_nat(find_big_map("frozen_extra_value", extras)) in
  let max_proposal_size = unpack_nat(find_big_map("max_proposal_size", extras)) in

  let required_token_lock = frozen_scale_value * proposal_size + frozen_extra_value in
  let has_correct_token_lock =
    (params.frozen_token = required_token_lock) && (proposal_size < max_proposal_size) in

  if has_correct_token_lock then
    match unpack_proposal_metadata(params.proposal_metadata) with
    | Transfer_proposal tp ->
        let min_xtz_amount = unpack_tez(find_big_map("min_xtz_amount", extras)) in
        let max_xtz_amount = unpack_tez(find_big_map("max_xtz_amount", extras)) in
        let is_all_transfers_valid (is_valid, transfer_type: bool * transfer_type) =
          match transfer_type with
          | Token_transfer_type tt -> is_valid
          | Xtz_transfer_type xt -> is_valid && min_xtz_amount <= xt.amount && xt.amount <= max_xtz_amount
        in
        List.fold is_all_transfers_valid tp.transfers has_correct_token_lock
    | Normal_proposal diff_ -> has_correct_token_lock
    | Update_receivers_proposal urp_ -> has_correct_token_lock
    | Configuration_proposal cp_ -> has_correct_token_lock

  else
    false
#elif VOTING_POWER_DAO
let registry_DAO_proposal_check (params, extras : propose_params * contract_extra) : bool =
  let proposal_size = Bytes.size(params.proposal_metadata) in
  let frozen_scale_value = unpack_nat(find_big_map("frozen_scale_value", extras)) in
  let frozen_extra_value = unpack_nat(find_big_map("frozen_extra_value", extras)) in
  let max_proposal_size = unpack_nat(find_big_map("max_proposal_size", extras)) in

  let required_token_lock = frozen_scale_value * proposal_size + frozen_extra_value in
  let has_correct_token_lock =
    (params.frozen_token = required_token_lock) && (proposal_size < max_proposal_size) in

  if has_correct_token_lock then
    match unpack_proposal_metadata(params.proposal_metadata) with
    | Transfer_proposal tp ->
        let min_xtz_amount = unpack_tez(find_big_map("min_xtz_amount", extras)) in
        let max_xtz_amount = unpack_tez(find_big_map("max_xtz_amount", extras)) in
        let is_all_transfers_valid (is_valid, transfer_type: bool * transfer_type) =
          match transfer_type with
          | Token_transfer_type tt -> is_valid
          | Xtz_transfer_type xt -> is_valid && min_xtz_amount <= xt.amount && xt.amount <= max_xtz_amount
        in
        List.fold is_all_transfers_valid tp.transfers has_correct_token_lock
    | Normal_proposal diff_ -> has_correct_token_lock
    | Update_receivers_proposal urp_ -> has_correct_token_lock
    | Configuration_proposal cp_ -> has_correct_token_lock
    | VotingPeriodConstantsUpdate cp_ -> has_correct_token_lock

  else
    false
#endif

(*
 * Proposal rejection return lambda: returns `slash_scale_value * frozen / slash_division_value`
 * where slash_scale_value and slash_division_value are specified by the DAO creator
 * in configuration and frozen is the amount that was frozen by the proposer.
 *)
let registry_DAO_rejected_proposal_return_value (params, extras : proposal * contract_extra) : nat =
  let slash_scale_value = unpack_nat(find_big_map ("slash_scale_value", extras)) in
  let slash_division_value = unpack_nat(find_big_map ("slash_division_value", extras))
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value

type update_fn = (address * address set) -> proposal_receivers

let update_receivers(current_set, updates, update_fn : proposal_receivers * address list * update_fn) : proposal_receivers =
  let
    foldFn (existing, item : proposal_receivers * address) : proposal_receivers = update_fn (item, existing)
  in List.fold foldFn updates current_set

#if BYO_DAO
(*
 * Uses the keys in the proposal metadata to differentiate between configuration
 * proposal and normal proposals. If the map contains `agoraPostID` key, the proposal
 * is considerd as 'normal'. Or else it is considered as carrying a 'configuration'
 * proposal.
 *
 * For 'normal' proposals, the contract_extra is expected to contain the bigmap
 * for registry and bigmap for key update tracking under keys 'registry' and
 * 'registry_affected' keys.
 *
 * For 'configuration' proposal, the code expects all the configuration keys
 * included in the proposal (or it throws an error). The expected keys are
 * "frozen_scale_value", "frozen_extra_value", "max_proposal_size", "slash_scale_value"
 * and "slash_division_value". 'None' values are ignored and 'Some' values are
 * used for updating corresponding configuraton.
 *)
let registry_DAO_decision_lambda (proposal, extras : proposal * contract_extra)
    : operation list * contract_extra =
  let propose_param : propose_params = {
    frozen_token = proposal.proposer_frozen_token;
    proposal_metadata = proposal.metadata
    } in
  let proposal_key = to_proposal_key (propose_param, proposal.proposer) in
  let ops = ([] : operation list) in
  match unpack_proposal_metadata(proposal.metadata) with
  | Normal_proposal np ->
      let extras = apply_diff(np.registry_diff, extras) in
      let extras =
        apply_diff_affected(proposal_key, np.registry_diff, extras) in
        (ops, extras)
  | Update_receivers_proposal urp ->
      let current_set = unpack_proposal_receivers(find_big_map("proposal_receivers", extras)) in
      begin
        let new_set = match urp with
          | Add_receivers receivers ->
              update_receivers(current_set, receivers, (fun (i, c : address * address set) -> Set.add i c))
          | Remove_receivers receivers ->
              update_receivers(current_set, receivers, (fun (i, c : address * address set) -> Set.remove i c))
        in (ops, Big_map.update "proposal_receivers" (Some (Bytes.pack new_set)) extras)
      end
  | Configuration_proposal cp ->
      let new_ce = match cp.frozen_scale_value with
        | Some (frozen_scale_value) ->
            Big_map.update "frozen_scale_value" (Some (Bytes.pack (frozen_scale_value))) extras
        | None -> extras in

      let new_ce = match cp.frozen_extra_value with
        | Some (frozen_extra_value) ->
            Big_map.update "frozen_extra_value" (Some (Bytes.pack (frozen_extra_value))) new_ce
        | None -> new_ce in

      let new_ce = match cp.max_proposal_size with
        | Some (max_proposal_size) ->
            Big_map.update "max_proposal_size" (Some (Bytes.pack (max_proposal_size))) new_ce
        | None -> new_ce in

      let new_ce = match cp.slash_scale_value with
        | Some (slash_scale_value) ->
            Big_map.update "slash_scale_value" (Some (Bytes.pack (slash_scale_value))) new_ce
        | None -> new_ce in

      let new_ce = match cp.slash_division_value with
        | Some (slash_division_value) ->
            Big_map.update "slash_division_value" (Some (Bytes.pack (slash_division_value))) new_ce
        | None -> new_ce
      in (ops, new_ce)
  | Transfer_proposal tp ->
      let handle_transfer (acc, transfer_type : (bool * contract_extra * operation list) * transfer_type) =
        let (is_valid, extras, ops) = acc in
        if is_valid then
          match transfer_type with
          | Token_transfer_type tt ->
            begin
              match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
                  : transfer_params contract option) with
              | Some contract ->
                  let token_transfer_operation = Tezos.transaction tt.transfer_list 0mutez contract
                  in (true, extras, token_transfer_operation :: ops)
              | None -> (false, extras, ops)
            end
          | Xtz_transfer_type xt ->
            begin
              match (Tezos.get_contract_opt xt.recipient : unit contract option) with
              | Some contract ->
                  let xtz_transfer_operation = Tezos.transaction unit xt.amount contract
                  in (true, extras, xtz_transfer_operation :: ops)
              | None -> (false, extras, ops)
            end
        else
          (false, extras, ops)
      in
      let (is_valid, extras, ops) = List.fold handle_transfer tp.transfers (true, extras, ops) in
      if is_valid then
        (ops, extras)
      else
        // TODO: [#87] Improve handling of failed proposals
        (failwith("FAIL_DECISION_LAMBDA") : operation list * contract_extra)
#elif VOTING_POWER_DAO
let registry_DAO_decision_lambda (proposal, extras : proposal * contract_extra)
    : operation list * (voting_period_params option * contract_extra) =
  let propose_param : propose_params = {
    frozen_token = proposal.proposer_frozen_token;
    proposal_metadata = proposal.metadata
    } in
  let proposal_key = to_proposal_key (propose_param, proposal.proposer) in
  let ops = ([] : operation list) in
  match unpack_proposal_metadata(proposal.metadata) with
  | Normal_proposal np ->
      let extras = apply_diff(np.registry_diff, extras) in
      let extras =
        apply_diff_affected(proposal_key, np.registry_diff, extras) in
        (ops, ((None : voting_period_params option), extras))
  | Update_receivers_proposal urp ->
      let current_set = unpack_proposal_receivers(find_big_map("proposal_receivers", extras)) in
      begin
        let new_set = match urp with
          | Add_receivers receivers ->
              update_receivers(current_set, receivers, (fun (i, c : address * address set) -> Set.add i c))
          | Remove_receivers receivers ->
              update_receivers(current_set, receivers, (fun (i, c : address * address set) -> Set.remove i c))
        in (ops, ((None : voting_period_params option), Big_map.update "proposal_receivers" (Some (Bytes.pack new_set)) extras))
      end
  | Configuration_proposal cp ->
      let new_ce = match cp.frozen_scale_value with
        | Some (frozen_scale_value) ->
            Big_map.update "frozen_scale_value" (Some (Bytes.pack (frozen_scale_value))) extras
        | None -> extras in

      let new_ce = match cp.frozen_extra_value with
        | Some (frozen_extra_value) ->
            Big_map.update "frozen_extra_value" (Some (Bytes.pack (frozen_extra_value))) new_ce
        | None -> new_ce in

      let new_ce = match cp.max_proposal_size with
        | Some (max_proposal_size) ->
            Big_map.update "max_proposal_size" (Some (Bytes.pack (max_proposal_size))) new_ce
        | None -> new_ce in

      let new_ce = match cp.slash_scale_value with
        | Some (slash_scale_value) ->
            Big_map.update "slash_scale_value" (Some (Bytes.pack (slash_scale_value))) new_ce
        | None -> new_ce in

      let new_ce = match cp.slash_division_value with
        | Some (slash_division_value) ->
            Big_map.update "slash_division_value" (Some (Bytes.pack (slash_division_value))) new_ce
        | None -> new_ce
      in (ops, ((None : voting_period_params option),  new_ce))
  | Transfer_proposal tp ->
      let handle_transfer (acc, transfer_type : (bool * contract_extra * operation list) * transfer_type) =
        let (is_valid, extras, ops) = acc in
        if is_valid then
          match transfer_type with
          | Token_transfer_type tt ->
            begin
              match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
                  : transfer_params contract option) with
              | Some contract ->
                  let token_transfer_operation = Tezos.transaction tt.transfer_list 0mutez contract
                  in (true, extras, token_transfer_operation :: ops)
              | None -> (false, extras, ops)
            end
          | Xtz_transfer_type xt ->
            begin
              match (Tezos.get_contract_opt xt.recipient : unit contract option) with
              | Some contract ->
                  let xtz_transfer_operation = Tezos.transaction unit xt.amount contract
                  in (true, extras, xtz_transfer_operation :: ops)
              | None -> (false, extras, ops)
            end
        else
          (false, extras, ops)
      in
      let (is_valid, extras, ops) = List.fold handle_transfer tp.transfers (true, extras, ops) in
      if is_valid then
        (ops, ((None : voting_period_params option), extras))
      else
        // TODO: [#87] Improve handling of failed proposals
        (failwith("FAIL_DECISION_LAMBDA") : operation list * (voting_period_params option * contract_extra))
  | VotingPeriodConstantsUpdate vpp ->
        (ops, (Some(vpp), extras))
#endif

// A custom entrypoint needed to receive xtz, since most `basedao` entrypoints
// prohibit non-zero xtz transfer.
let receive_xtz_entrypoint (params, full_store : bytes * full_storage) : return =
  (([]: operation list), full_store.0)

// A custom entrypoint to fetch values from Registry
let lookup_registry (bytes_param, full_store : bytes * full_storage) : operation list * storage =
  let param : lookup_registry_param = unpack_lookup_registry_param ("lookup_registry_param", bytes_param) in
  let view_contract : lookup_registry_view =
      match (Tezos.get_contract_opt(param.callback) : lookup_registry_view option) with
      | Some callback_contract -> callback_contract
      | None -> (failwith "BAD_VIEW_CONTRACT": lookup_registry_view) in
  let contract_extra = full_store.0.extra in
  let registry : registry = unpack_registry(find_big_map("registry", contract_extra)) in
  let value_at_key : registry_value option = Big_map.find_opt param.key registry in
  let operation : operation = Tezos.transaction (param.key, value_at_key) 0mutez view_contract
  in ((operation :: ([]: operation list)), full_store.0)

let default_registry_DAO_full_storage (data : initial_registryDAO_storage) : full_storage =
  let (store, config) = default_full_storage (data.base_data) in
  let new_storage = { store with
    extra = Big_map.literal [
          ("registry" , Bytes.pack (Map.empty : registry));
          ("registry_affected" , Bytes.pack (Map.empty : registry_affected));
          ("proposal_receivers" , Bytes.pack (Set.empty : proposal_receivers));
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
    proposal_check = registry_DAO_proposal_check;
    rejected_proposal_return_value = registry_DAO_rejected_proposal_return_value;
    decision_lambda = registry_DAO_decision_lambda;
    custom_entrypoints = Big_map.literal
      [ "lookup_registry", Bytes.pack lookup_registry
      ; "receive_xtz", Bytes.pack receive_xtz_entrypoint
      ];
    } in
  (new_storage, new_config)

// We are not using this right now, but just leaving here in case we might want it
// soon.
let successful_proposal_receiver_view (full_storage : full_storage): proposal_receivers =
  match ((Big_map.find_opt "proposal_receivers" full_storage.0.extra) : bytes option) with
    | Some (packed_b) ->
        begin
          match (Bytes.unpack packed_b : proposal_receivers option) with
          | Some r -> r
          | None -> ((failwith "Unpacking failed") : proposal_receivers)
        end
    | None -> (failwith "'proposal_receivers' key not found" : proposal_receivers)
