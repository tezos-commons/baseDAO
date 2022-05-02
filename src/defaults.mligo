// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "types.mligo"
#include "proposal/quorum_threshold.mligo"

let validate_proposal_flush_expired_level (data : initial_config_data) : unit =
  if data.proposal_expired_level.blocks <= data.proposal_flush_level.blocks then
    failwith("proposal_expired_level needs to be bigger than proposal_flush_level")
  else if data.proposal_flush_level.blocks < (to_basedao_period(data.voting_period_params) * 2n) then
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
        { current_cycle_num = 0n
        ; frozen_tokens = 0n
        } in
  ( Big_map.add key entry freeze_history
  , total + amt
  )

let default_config (data : initial_config_data) : config =
  let _ : unit = validate_proposal_flush_expired_level(data) in
  let _ : unit = validate_quorum_threshold_bound(data) in {
    fixed_proposal_fee_in_token = data.fixed_proposal_fee_in_token;
    voting_period_params = data.voting_period_params;
    max_quorum_threshold = to_signed(data.max_quorum);
    min_quorum_threshold = to_signed(data.min_quorum);
    max_quorum_change = to_signed(data.max_quorum_change);
    quorum_change = to_signed(data.quorum_change);
    governance_total_supply = data.governance_total_supply;
    proposal_flush_level = data.proposal_flush_level;
    proposal_expired_level = data.proposal_expired_level;
  }

let default_storage (data: initial_data) : storage =
  let quorum_threshold =
        bound_qt
          (  to_signed(data.config_data.quorum_threshold)
          ,  to_signed(data.config_data.min_quorum)
          ,  to_signed(data.config_data.max_quorum) ) in
  let frozen_token_id: nat = 0n in
  let (freeze_history, total) =
    List.fold freeze_history_constructor data.freeze_history ((Big_map.empty : freeze_history), 0n) in
  {
    governance_token = data.governance_token;
    admin = data.admin;
    guardian = data.guardian;
    pending_owner = data.admin;
    metadata = data.metadata_map;
    extra = default_extra;
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    ongoing_proposals_dlist = (None : proposal_doubly_linked_list option);
    staked_votes = (Big_map.empty : (address * proposal_key, staked_vote) big_map);
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
    config = default_config (data.config_data);
  }
