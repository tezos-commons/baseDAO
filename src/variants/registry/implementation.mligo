// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT
#define VARIANT

#include "error_codes.mligo"
#include "common/errors.mligo"
#include "common/types.mligo"
#include "defaults.mligo"
#include "types.mligo"
#include "proposal/helpers.mligo"
#include "helper/unpack.mligo"

#include "variants/registry/types.mligo"

type custom_ep_param = custom_ep_param_private

let apply_diff_registry (diff, registry : registry_diff * registry) : registry =
  let
    foldFn (registry, update: registry * (registry_key * registry_value option)) : registry =
      let (registry_key, registry_value) = update in
      Big_map.update registry_key registry_value registry
  in List.fold foldFn diff registry

let apply_diff_registry_affected
  (proposal_key, diff, registry_affected : proposal_key * registry_diff * registry_affected)
    : registry_affected =
  let
    foldFn (registry_affected, update: registry_affected * (registry_key *
      registry_value option)) : registry_affected =
      let registry_key = update.0 in
      Big_map.update registry_key (Some (proposal_key)) registry_affected
  in List.fold foldFn diff registry_affected

let apply_diff (diff, ce : registry_diff * contract_extra) : contract_extra =
  let registry = ce.registry in
  let new_registry : registry = apply_diff_registry (diff, registry) in
  { ce with registry = new_registry }

let apply_diff_affected
  (proposal_key, diff, ce : proposal_key * registry_diff * contract_extra)
    : contract_extra =
  let registry_af = ce.registry_affected in
  let new_registry_af : registry_affected = apply_diff_registry_affected (proposal_key, diff, registry_af)
  in { ce with registry_affected = new_registry_af }

(*
 * Proposal check callback : returns true if following checks are true.
 * 1. if proposer has locked frozen_scale_value * s + frozen_extra_value
 * where s = size(pack(proposalMetadata)) and frozen_scale_value and frozen_extra_value
 * are from storage configuration.
 * 2. proposal size < max_proposal_size where max_proposal_size is from stored configuration.
 * 3. if transfers is present in the metadata, then it contains transfers such that each transfer:
 *   3.a. is a xtz transfer satisfying min_xtz_amount <= amount <= max_xtz_amount
 *   3.b. is a token transfer
 * where min_xtz_amount and max_xtz_amount are from storage configuration.
 *)
let proposal_check (params, extras : propose_params * contract_extra) : unit =
  let proposal_size = Bytes.length(params.proposal_metadata) in
  let frozen_scale_value = extras.frozen_scale_value in
  let frozen_extra_value = extras.frozen_extra_value in
  let max_proposal_size = extras.max_proposal_size in

  let required_token_lock = frozen_scale_value * proposal_size + frozen_extra_value in

  let _ : unit =
    match check_token_locked_and_proposal_size(params.frozen_token, required_token_lock, proposal_size, max_proposal_size) with
    | Some err_msg -> fail_proposal_check(err_msg)
    | None -> unit in

  match unpack_proposal_metadata(params.proposal_metadata) with
  | Transfer_proposal tp ->
      let min_xtz_amount = extras.min_xtz_amount in
      let max_xtz_amount = extras.max_xtz_amount in
      let is_all_transfers_valid (transfer_type: transfer_type) =
        match transfer_type with
        | Legacy_token_transfer_type _tt -> unit
        | Token_transfer_type _tt -> unit
        | Xtz_transfer_type xt ->
            begin
              match check_xtz_transfer(xt, min_xtz_amount, max_xtz_amount) with
              | Some err_msg -> fail_proposal_check(err_msg)
              | None -> unit
            end
      in
        List.iter is_all_transfers_valid tp.transfers
  | Update_receivers_proposal _urp -> unit
  | Configuration_proposal _cp -> unit
  | Update_guardian _guardian -> unit
  | Update_contract_delegate _ -> unit

(*
 * Proposal rejection slash callback: returns `slash_scale_value * frozen / slash_division_value`
 * where slash_scale_value and slash_division_value are specified by the DAO creator
 * in configuration and frozen is the amount that was frozen by the proposer.
 *)
let rejected_proposal_slash_value (params, extras : proposal * contract_extra) : nat =
  let slash_scale_value = extras.slash_scale_value in
  let slash_division_value = extras.slash_division_value
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value

type update_fn = (address set * address) -> proposal_receivers

let update_receivers(current_set, updates, update_fn : proposal_receivers * address list * update_fn)
    : proposal_receivers =
  List.fold update_fn updates current_set

let handle_transfer (ops, transfer_type : (operation list) * transfer_type) : (operation list) =
  match transfer_type with
  | Legacy_token_transfer_type tt ->
    begin
      match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
          : (legacy_transfer contract) option) with
      | Some contract ->
          (Tezos.transaction tt.transfer 0mutez contract) :: ops
      | None -> (failwith fail_decision_callback : operation list)
    end
  | Token_transfer_type tt ->
    begin
      match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
          : (transfer_params contract) option) with
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

(*
 * Uses the keys in the proposal metadata to differentiate between configuration
 * proposal and normal proposals. If the map contains `agoraPostID` key, the proposal
 * is considerd as 'normal'. Or else it is considered as carrying a 'configuration'
 * proposal.
 *
 * For 'normal' proposals, the contract_extra is expected to contain the bigmap
 * for registry and bigmap for key update tracking under keys `registry` and
 * `registry_affected`.
 *
 * For 'configuration' proposal, the code expects all the configuration keys
 * included in the proposal (or it throws an error). The expected keys are
 * `frozen_scale_value`, `frozen_extra_value`, `max_proposal_size`, `slash_scale_value`
 * and `slash_division_value`. `None` values are ignored and `Some` values are
 * used for updating corresponding configuraton.
 *)
let decision_callback (input : decision_callback_input)
    : decision_callback_output =
  let (proposal, extras) = (input.proposal, input.extras) in
  let propose_param : propose_params = {
    from = proposal.proposer;
    frozen_token = proposal.proposer_frozen_token;
    proposal_metadata = proposal.metadata
    } in
  let proposal_key = to_proposal_key (propose_param) in
  let ops = ([] : operation list) in
  match unpack_proposal_metadata(proposal.metadata) with
  | Update_receivers_proposal urp ->
      let current_set = extras.proposal_receivers in
      let new_set = match urp with
        | Add_receivers receivers ->
            update_receivers(current_set, receivers, (fun (c, i : address set * address) -> Set.add i c))
        | Remove_receivers receivers ->
            update_receivers(current_set, receivers, (fun (c, i : address set * address) -> Set.remove i c))
      in { operations = ops
         ; extras = { extras with proposal_receivers = new_set }
         ; guardian = (None : (address option))
         }
  | Configuration_proposal cp ->
      let new_ce = match cp.frozen_scale_value with
        | Some (frozen_scale_value) ->
            { extras with frozen_scale_value = frozen_scale_value }
        | None -> extras in

      let new_ce = match cp.frozen_extra_value with
        | Some (frozen_extra_value) ->
            { extras with frozen_extra_value = frozen_extra_value }
        | None -> new_ce in

      let new_ce = match cp.max_proposal_size with
        | Some (max_proposal_size) ->
            { extras with max_proposal_size = max_proposal_size }
        | None -> new_ce in

      let new_ce = match cp.slash_scale_value with
        | Some (slash_scale_value) ->
            { extras with slash_scale_value = slash_scale_value }
        | None -> new_ce in

      let new_ce = match cp.slash_division_value with
        | Some (slash_division_value) ->
            { extras with slash_division_value = slash_division_value }
        | None -> new_ce
      in { operations = ops; extras = new_ce; guardian = (None : (address option)) }
  | Transfer_proposal tp ->
      // handle updates
      let registry_diff = tp.registry_diff in
      let extras = apply_diff(registry_diff, extras) in
      let extras = apply_diff_affected(proposal_key, registry_diff, extras) in

      // handle transfers
      let transfers = tp.transfers in
      let ops = List.fold handle_transfer transfers ops in
      { operations = ops; extras = extras; guardian = (None : (address option)) }
  | Update_guardian guardian ->
      { operations = ops; extras = extras ; guardian = Some(guardian) }
  | Update_contract_delegate mdelegate ->
      { operations = ((Tezos.set_delegate mdelegate) :: ops); extras = extras ; guardian = (None : (address option))}

// A custom entrypoint to fetch values from Registry
let lookup_registry (param, store : lookup_registry_param * storage) : operation list * storage =
  let view_contract : lookup_registry_view =
      match (Tezos.get_contract_opt(param.callback) : lookup_registry_view option) with
      | Some callback_contract -> callback_contract
      | None -> (failwith bad_view_contract: lookup_registry_view) in
  let contract_extra = store.extra in
  let registry : registry = contract_extra.registry in
  let value_at_key : registry_value option = Big_map.find_opt param.key registry in
  let operation : operation = Tezos.transaction (param.key, value_at_key) 0mutez view_contract
  in ((operation :: nil_op), store)

let default_registry_DAO_storage (data : initial_registryDAO_storage) : storage =
  let store = default_storage (data.base_data) in
  { store with
    extra =
      { default_extra with frozen_scale_value = data.frozen_scale_value
      ; frozen_extra_value = data.frozen_extra_value
      ; max_proposal_size = data.max_proposal_size
      ; slash_scale_value = data.slash_scale_value
      ; slash_division_value = data.slash_division_value
      ; min_xtz_amount = data.min_xtz_amount
      ; max_xtz_amount = data.max_xtz_amount
      }
  }

// We are not using this right now, but just leaving here in case we might want it
// soon.
let successful_proposal_receiver_view (storage : storage): proposal_receivers =
  storage.extra.proposal_receivers

let custom_ep (custom_ep_param, storage : custom_ep_param * storage): operation list * storage
  = match custom_ep_param with
  | Lookup_registry p -> lookup_registry(p, storage)
  | _ -> (([]: operation list), storage)

#endif
