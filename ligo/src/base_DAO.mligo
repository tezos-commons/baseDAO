// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to BaseDAO.hs module

#include "management.mligo"
#include "permit.mligo"
#include "proposal.mligo"
#include "token.mligo"

let base_DAO_contract
  (action, full_store : parameter * full_storage)
    : operation list * full_storage
  =
  let store : storage = full_store.0 in
  let config : config = full_store.1 in
  match action with
  // entrypoints that require the contract to not be migrated
    M_left (mp) ->
      begin
        let store = ensure_not_migrated(store)
        in match mp with
            // entrypoint that won't necessarily check for xtz
              M_left (p) -> call_custom(p, full_store)
            // entrypoints that won't accept any xtz
            | M_right (fbp) ->
                let result =
                  if Tezos.amount > 0tez
                    then (failwith("FORBIDDEN_XTZ"): return)
                    else
                      begin
                        match fbp with
                            Call_FA2 (p) -> call_fa2(p, store)
                          | Drop_proposal (p) -> drop_proposal(p, config, store)
                          | Transfer_ownership (p) -> transfer_ownership(p, store)
                          | Accept_ownership (p) -> accept_ownership(p, store)
                          | Migrate (p) -> migrate(p, store)
                          | Confirm_migration (p) -> confirm_migration(p, store)
                          | Propose (p) -> propose(p, config, store)
                          | Vote (p) -> vote(p, config, store)
                          | Set_voting_period (p) -> set_voting_period(p, config, store)
                          | Set_quorum_threshold (p) -> set_quorum_threshold(p, config, store)
                          | Flush (p) -> flush (p, config, store)
                          | Burn (p) -> burn(p, store)
                          | Mint (p) -> mint(p, store)
                          | GetVotePermitCounter (p) -> get_vote_permit_counter (p, store)
                      end
                in (result.0, (result.1, config))
      end
  // entrypoint that also work when the contract has been migrated
  | M_right (p) -> let result = transfer_contract_tokens(p, store) in
      (result.0, (result.1, config))
