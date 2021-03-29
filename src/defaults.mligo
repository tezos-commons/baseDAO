// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"

let default_config : config = {
    proposal_check = (fun (params, extras : propose_params * contract_extra) -> true);
    rejected_proposal_return_value = (fun (proposal, extras : proposal * contract_extra) -> 0n);
    decision_lambda = (fun (proposal, extras : proposal * contract_extra) -> (([] : (operation list)), extras));

    max_proposals = 500n;
    max_votes = 1000n;
    max_quorum_threshold = 1000n;
    min_quorum_threshold = 1n;
    max_voting_period = 2592000n; // 60 * 60 * 24 * 30
    min_voting_period = 1n;
    custom_entrypoints = (Map.empty : custom_entrypoints);
    }

let default_storage (admin , token_address , now_val, metadata : address * address * timestamp * metadata_map) : storage = {
    ledger = (Big_map.empty : ledger);
    operators = (Big_map.empty : operators);
    token_address = token_address;
    admin = admin;
    pending_owner = admin;
    metadata = metadata;
    migration_status = Not_in_migration;
    voting_period = 11n;
    quorum_threshold = 2n;
    extra = (Map.empty : (string, bytes) map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_date = (Set.empty : (timestamp * proposal_key) set);
    permits_counter = 0n;
    freeze_history = (Big_map.empty : freeze_history);
    total_supply = Map.literal
        [ (frozen_token_id, 0n)
        ; (unfrozen_token_id, 0n)
        ];
    fixed_proposal_fee_in_token = 0n;
    frozen_token_id = frozen_token_id;
    unfrozen_token_id = unfrozen_token_id;
    last_period_change = {changed_on = now_val; period_num = 0n}
}

let default_full_storage (admin, token_address, now_val, metadata_map : address * address * timestamp * metadata_map) : full_storage =
  (default_storage (admin, token_address, now_val, metadata_map), default_config)
