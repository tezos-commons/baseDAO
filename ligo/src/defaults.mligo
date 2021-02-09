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
    }

let default_storage (admin , token_address : (address * address)) : storage = {
    ledger = (Big_map.empty : ledger);
    operators = (Big_map.empty : operators);
    token_address = token_address;
    admin = admin;
    pending_owner = admin;
    metadata = (Big_map.empty : (string, bytes) big_map);
    migration_status = Not_in_migration;
    voting_period = 11n;
    quorum_threshold = 2n;
    extra = (Map.empty : (string, bytes) map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_date = (Set.empty : (timestamp * proposal_key) set);
    permits_counter = 0n;
}

let default_startup_storage : startup_storage = {
    starting_up = true;
    stored_entrypoints = (Big_map.empty : stored_entrypoints);
    }

let default_full_storage (admin, token_address : (address * address)) : full_storage =
  (default_startup_storage, (default_storage (admin, token_address), default_config))
