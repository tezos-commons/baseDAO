// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"
#include "proposal.mligo"

let validate_fixed_proposal_fee_in_token (init_conf : initial_config_data): nat =
  match init_conf.fixed_proposal_fee_in_token with
    | Some a -> a
    | None -> failwith("'fixed_proposal_fee_in_token' cannot be empty.")

let validate_min_max_quorum_threshold (init_conf : initial_config_data): (quorum_threshold * quorum_threshold) =
  match (init_conf.max_quorum, init_conf.min_quorum) with
    | (Some a, Some b) ->
        if a <= b
        then failwith("'max_quorum' cannot be smaller than 'min_quorum'")
        else ( { numerator = (a * quorum_denominator)/100n }
             , { numerator = (a * quorum_denominator)/100n }
             )
    | _ -> failwith("'max_quorum' or 'min_quorum' cannot be empty.")

let validate_quorum_change (init_conf : initial_config_data): (unsigned_quorum_fraction * unsigned_quorum_fraction) =
  match (init_conf.max_quorum_change, init_conf.quorum_change) with
    | (Some a, Some b) ->
        if a <= b
        then failwith("'max_quorum_change' cannot be smaller than 'quorum_change'")
        else ( { numerator = (a * quorum_denominator)/100n }
             , { numerator = (a * quorum_denominator)/100n }
             )
    | _ -> failwith("'max_quorum_change' or 'quorum_change' cannot be empty.")

let validate_governance_total_supply (init_conf : initial_config_data): nat =
  match init_conf.governance_total_supply with
    | Some a -> a
    | None -> failwith("'governance_total_supply' cannot be empty.")

let validate_proposal_flush_expired_time_voting_period (init_conf : initial_config_data): (seconds * seconds * voting_period) =
  match (init_conf.proposal_flush_time, init_conf.proposal_expired_time, init_conf.voting_period) with
    | (Some flushed, Some expired, Some voting_period) ->
      if expired <= flushed then
        failwith("'proposal_expired_time' needs to be bigger than 'proposal_flush_time'")
      else if flushed <= (voting_period * 2n) then
        failwith("'proposal_flush_time' needs to be more than twice the 'voting_period'")
      else (flushed, expired, { length = voting_period })
    | _ -> failwith("'proposal_flush_time', 'proposal_expired_time' or 'voting_period' cannot be empty.")


let validate_quorum_threshold (init_conf : initial_config_data): quorum_threshold =
  match init_conf.quorum_threshold with
    | Some a -> { numerator = (a * quorum_denominator)/100n }
    | None -> failwith("'quorum_threshold' cannot be empty.")

let validate_config (init_conf : initial_config_data) : config =
  let fixed_proposal_fee_in_token = validate_fixed_proposal_fee_in_token (init_conf) in
  let (max_quorum_threshold, min_quorum_threshold) = validate_min_max_quorum_threshold (init_conf) in
  let (max_quorum_change, quorum_change) = validate_quorum_change (init_conf) in
  let governance_total_supply = validate_governance_total_supply (init_conf) in
  let (proposal_flush_time, proposal_expired_time, voting_period) = validate_proposal_flush_expired_time_voting_period (init_conf) in
  {
    proposal_check = (fun (_params, _extras : propose_params * contract_extra) -> true);
    rejected_proposal_return_value = (fun (_proposal, _extras : proposal * contract_extra) -> 0n);
    decision_lambda = (fun (_proposal, extras : proposal * contract_extra) -> (([] : (operation list)), extras));
    fixed_proposal_fee_in_token = fixed_proposal_fee_in_token;
    voting_period = voting_period;
    max_proposals = 500n;
    max_votes = 1000n;
    max_quorum_threshold = to_signed(max_quorum_threshold);
    min_quorum_threshold = to_signed(min_quorum_threshold);
    max_quorum_change = to_signed(max_quorum_change);
    quorum_change = to_signed(quorum_change);
    governance_total_supply = governance_total_supply;
    proposal_flush_time = proposal_flush_time;
    proposal_expired_time = proposal_expired_time;
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

let validate_ledger_lst (init_store : initial_storage_data): ledger_list =
  match init_store.ledger_lst with
    | Some a -> a
    | None -> failwith("'ledger_lst' cannot be empty.")

let validate_governance_token (init_store : initial_storage_data): governance_token =
  match (init_store.governance_token_address, init_store.governance_token_id) with
    | (Some addr, Some token_id) -> { address = addr; token_id = token_id }
    | _ -> failwith("'governance_token' cannot be empty.")

let validate_admin (init_store : initial_storage_data): address =
  match init_store.admin with
    | Some a -> a
    | None -> failwith("'admin' cannot be empty.")

let validate_guardian (init_store : initial_storage_data): address =
  match init_store.guardian with
    | Some a -> a
    | None -> failwith("'guardian' cannot be empty.")

let validate_metadata (init_store : initial_storage_data): metadata_map =
  match init_store.metadata_map with
    | Some a -> a
    | None -> failwith("'metadata_map' cannot be empty.")

let validate_now_val (init_store : initial_storage_data): timestamp =
  match init_store.now_val with
    | Some a -> a
    | None -> failwith("'now_val' cannot be empty.")


let validate_storage (init_store, init_conf, config : initial_storage_data * initial_config_data * config ) : storage =
  let ledger_lst = validate_ledger_lst (init_store) in
  let governance_token = validate_governance_token (init_store) in
  let admin = validate_admin (init_store) in
  let guardian = validate_guardian (init_store) in
  let metadata = validate_metadata (init_store) in
  let now_val = validate_now_val (init_store) in


  let quorum_threshold_config = to_signed (validate_quorum_threshold(init_conf)) in
  let quorum_threshold =
        bound_qt
          (  quorum_threshold_config
          ,  config.min_quorum_threshold
          ,  config.max_quorum_threshold ) in
  let frozen_token_id: nat = 0n in
  {
    ledger = List.fold ledger_constructor ledger_lst (Big_map.empty : ledger);
    operators = (Big_map.empty : operators);
    governance_token = governance_token;
    admin = admin;
    guardian = guardian;
    pending_owner = admin;
    metadata = metadata;
    extra = (Big_map.empty : (string, bytes) big_map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_date = (Set.empty : (timestamp * proposal_key) set);
    permits_counter = 0n;
    freeze_history = (Big_map.empty : freeze_history);
    total_supply = List.fold total_supply_constructor ledger_lst (Map.literal
      [ (frozen_token_id, 0n)
      ]
    );
    frozen_token_id = frozen_token_id;
    start_time = now_val;
    quorum_threshold_at_cycle =
      { last_updated_cycle = 1n
      // We use 1 here so that the initial quorum will be used for proposals raised in period 1
      // as there is no meaningful participation before that.
      ; quorum_threshold = to_unsigned(quorum_threshold)
      ; staked = 0n
      };
  }


let default_full_storage (init : initial_data) : full_storage =
  let config = validate_config (init.config_data) in
  let store = validate_storage (init.storage_data, init.config_data, config) in
  (store, config)
