// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"
#include "proposal.mligo"

let validate_proposal_flush_expired_level (data : initial_config_data) : unit =
  if data.proposal_expired_level.blocks <= data.proposal_flush_level.blocks then
    failwith("proposal_expired_level needs to be bigger than proposal_flush_level")
  else if data.proposal_flush_level.blocks < (data.period.blocks * 2n) then
    failwith("proposal_flush_level needs to be more than twice the 'period' length.")
  else unit

let validate_quorum_threshold_bound (data : initial_config_data) : unit =
  if data.quorum_threshold >= data.max_quorum then
    failwith("'quorum_threshold' needs to be smaller than or equal to 'max_quorum'")
  else if data.quorum_threshold <= data.min_quorum then
    failwith("'quorum_threshold' needs to be bigger than or equal to 'min_quorum'")
  else
    unit

let freeze_history_constructor (acc, param : (freeze_history * nat) * (address * nat)) : (freeze_history * nat) =
  let (freeze_history, total) = acc in
  let (key, amt) = param in
  let entry : address_freeze_history =
        { current_stage_num = 0n
        ; staked = 0n
        ; current_unstaked = amt
        ; past_unstaked = 0n
        } in
  ( Big_map.add key entry freeze_history
  , total + amt
  )

let default_config (data : initial_config_data) : config =
  let _ : unit = validate_proposal_flush_expired_level(data) in
  let _ : unit = validate_quorum_threshold_bound(data) in {
    // TODO [#271] We have to find a safe value for max_voters
    // and check if the value contained in the `data` is
    // within bounds.
    proposal_check = (fun (_params, _extras : propose_params * contract_extra) -> unit);
    rejected_proposal_slash_value = (fun (_proposal, _extras : proposal * contract_extra) -> 0n);
    decision_lambda = (fun (dl_input : decision_lambda_input) ->
      { operations = ([] : (operation list)); extras = dl_input.extras; guardian = (None : (address option))});
    fixed_proposal_fee_in_token = data.fixed_proposal_fee_in_token;
    period = data.period;
    max_proposals = 500n;
    max_voters = data.max_voters;
    max_quorum_threshold = to_signed(data.max_quorum);
    min_quorum_threshold = to_signed(data.min_quorum);
    max_quorum_change = to_signed(data.max_quorum_change);
    quorum_change = to_signed(data.quorum_change);
    governance_total_supply = data.governance_total_supply;
    proposal_flush_level = data.proposal_flush_level;
    proposal_expired_level = data.proposal_expired_level;
    custom_entrypoints = (Big_map.empty : custom_entrypoints);
  }


let default_storage (data, config_data : initial_storage_data * initial_config_data ) : storage =
  let quorum_threshold =
        bound_qt
          (  to_signed(config_data.quorum_threshold)
          ,  to_signed(config_data.min_quorum)
          ,  to_signed(config_data.max_quorum) ) in
  let frozen_token_id: nat = 0n in
  let (freeze_history, total) =
    List.fold freeze_history_constructor data.freeze_history ((Big_map.empty : freeze_history), 0n) in
  {
    governance_token = data.governance_token;
    admin = data.admin;
    guardian = data.guardian;
    pending_owner = data.admin;
    metadata = data.metadata_map;
    extra = (Big_map.empty : (string, bytes) big_map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_level = (Set.empty : (blocks * proposal_key) set);
    permits_counter = 0n;
    freeze_history = freeze_history;
    frozen_token_id = frozen_token_id;
    start_level = data.start_level;
    quorum_threshold_at_cycle =
      { last_updated_cycle = 1n
      // We use 1 here so that the initial quorum will be used for proposals raised in stage 1
      // as there is no meaningful participation before that.
      ; quorum_threshold = to_unsigned(quorum_threshold)
      ; staked = 0n
      };
    frozen_total_supply = total;
    delegates = (Big_map.empty : delegates);
  }

let default_full_storage (data : initial_data) : full_storage =
  ( default_storage (data.storage_data, data.config_data), default_config (data.config_data) )
