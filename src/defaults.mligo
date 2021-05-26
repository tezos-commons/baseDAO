// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"
#include "proposal.mligo"

let validate_proposal_flush_expired_time (data : initial_config_data) : unit =
  if data.proposal_expired_time <= data.proposal_flush_time then
    failwith("'proposal_expired_time' needs to be bigger than 'proposal_flush_time'.")
  else if data.proposal_flush_time <= (data.period.length * 2n) then
    failwith("'proposal_flush_time' needs to be more than twice the 'period' length.")
  else unit

let validate_max_votes (data : initial_config_data) : unit =
  if data.max_votes > data.governance_total_supply then
    failwith("The 'max_votes' number cannot exceed the 'governance_total_supply'.")
  // TODO #271: check for maximum reasonable value
  else unit

let validate_quorum_threshold_bound (data : initial_config_data) : unit =
  if data.quorum_threshold >= data.max_quorum then
    failwith("'quorum_threshold' needs to be smaller than or equal to 'max_quorum'")
  else if data.quorum_threshold <= data.min_quorum then
    failwith("'quorum_threshold' needs to be bigger than or equal to 'min_quorum'")
  else
    unit

let default_config (data : initial_config_data) : config =
  let _ : unit = validate_proposal_flush_expired_time(data) in
  let _ : unit = validate_max_votes(data) in
  let _ : unit = validate_quorum_threshold_bound(data) in {
    proposal_check = (fun (_params, _extras : propose_params * contract_extra) -> true);
    rejected_proposal_slash_value = (fun (_proposal, _extras : proposal * contract_extra) -> 0n);
    decision_lambda = (fun (_proposal, extras : proposal * contract_extra) -> (([] : (operation list)), extras));
    fixed_proposal_fee_in_token = data.fixed_proposal_fee_in_token;
    period = data.period;
    max_proposals = 500n;
    max_votes = data.max_votes;
    max_quorum_threshold = to_signed(data.max_quorum);
    min_quorum_threshold = to_signed(data.min_quorum);
    max_quorum_change = to_signed(data.max_quorum_change);
    quorum_change = to_signed(data.quorum_change);
    governance_total_supply = data.governance_total_supply;
    proposal_flush_time = data.proposal_flush_time;
    proposal_expired_time = data.proposal_expired_time;
    custom_entrypoints = (Big_map.empty : custom_entrypoints);
  }

let ledger_constructor (ledger, param : ledger * (ledger_key * ledger_value)) : ledger =
  let (key, value) = param in
  Big_map.add key value ledger

let total_supply_constructor (total_supply, param : total_supply * (ledger_key * ledger_value)) : total_supply =
  let (key, value) = param in
  let token_id = key.1 in
  match Map.find_opt token_id total_supply with
    | None -> Map.add token_id value total_supply
    | Some v -> Map.add token_id (v + value) total_supply

let default_storage (data, config_data : initial_storage_data * initial_config_data ) : storage =
  let quorum_threshold =
        bound_qt
          (  to_signed(config_data.quorum_threshold)
          ,  to_signed(config_data.min_quorum)
          ,  to_signed(config_data.max_quorum) ) in
  let frozen_token_id: nat = 0n in
  {
    ledger = List.fold ledger_constructor data.ledger_lst (Big_map.empty : ledger);
    operators = (Big_map.empty : operators);
    governance_token = data.governance_token;
    admin = data.admin;
    guardian = data.guardian;
    pending_owner = data.admin;
    metadata = data.metadata_map;
    extra = (Big_map.empty : (string, bytes) big_map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_date = (Set.empty : (timestamp * proposal_key) set);
    permits_counter = 0n;
    freeze_history = (Big_map.empty : freeze_history);
    total_supply = List.fold total_supply_constructor data.ledger_lst (Map.literal
      [ (frozen_token_id, 0n)
      ]
    );
    frozen_token_id = frozen_token_id;
    start_time = data.now_val;
    quorum_threshold_at_cycle =
      { last_updated_cycle = 1n
      // We use 1 here so that the initial quorum will be used for proposals raised in stage 1
      // as there is no meaningful participation before that.
      ; quorum_threshold = to_unsigned(quorum_threshold)
      ; staked = 0n
      };
  }

let default_full_storage (data : initial_data) : full_storage =
  ( default_storage (data.storage_data, data.config_data), default_config (data.config_data) )
