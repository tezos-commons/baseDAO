// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"
#include "proposal.mligo"

let default_config (data : initial_config_data) : config = {
  proposal_check = (fun (params, extras : propose_params * contract_extra) -> true);
  rejected_proposal_return_value = (fun (proposal, extras : proposal * contract_extra) -> 0n);
  decision_lambda = (fun (proposal, extras : proposal * contract_extra) -> (([] : (operation list)), extras));

  max_proposals = 500n;
  max_votes = 1000n;
  max_voting_period = data.max_period;
  min_voting_period = data.min_period;

  custom_entrypoints = (Big_map.empty : custom_entrypoints);
}

let bound_vp (vp, min_vp, max_vp : voting_period * voting_period * voting_period)
    : voting_period =
  if vp < min_vp
  then min_vp
  else
    if vp > max_vp
    then max_vp
    else vp

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
  let frozen_token_id: nat = 0n in
  {
    ledger = List.fold ledger_constructor data.ledger_lst (Big_map.empty : ledger);
    operators = (Big_map.empty : operators);
    governance_token = data.governance_token;
    admin = data.admin;
    pending_owner = data.admin;
    metadata = data.metadata_map;
    voting_period = bound_vp ( data.voting_period, config_data.min_period, config_data.max_period );
    quorum_threshold = data.quorum_threshold;
    extra = (Big_map.empty : (string, bytes) big_map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_date = (Set.empty : (timestamp * proposal_key) set);
    permits_counter = 0n;
    freeze_history = (Big_map.empty : freeze_history);
    total_supply = List.fold total_supply_constructor data.ledger_lst (Map.literal
      [ (frozen_token_id, 0n)
      ]
    );
    fixed_proposal_fee_in_token = 0n;
    frozen_token_id = frozen_token_id;
    last_period_change = {changed_on = data.now_val; period_num = 0n}
}

let default_full_storage (data : initial_data) : full_storage =
  ( default_storage (data.storage_data, data.config_data), default_config (data.config_data) )
