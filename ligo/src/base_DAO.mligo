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
  let result = match action with
        Call_FA2 (p) -> call_fa2(p, store)
      | CallCustom (p) -> call_custom(p, store)
      | Drop_proposal (p) -> drop_proposal(p, store)
      | Transfer_ownership (p) -> transfer_ownership(p, store)
      | Accept_ownership (p) -> accept_ownership(p, store)
      | Migrate (p) -> migrate(p, store)
      | Confirm_migration (p) -> confirm_migration(p, store)
      | Propose (p) -> propose(p, store)
      | Vote (p) -> vote(p, store)
      | Set_voting_period (p) -> set_voting_period(p, store)
      | Set_quorum_threshold (p) -> set_quorum_threshold(p, store)
      | Flush (p) -> flush (p, config, store)
      | Burn (p) -> burn(p, store)
      | Mint (p) -> mint(p, store)
      | Transfer_contract_tokens (p) -> transfer_contract_tokens(p, store)
      | GetVotePermitCounter (p) -> (failwith("not necessary") : return)
  in (result.0, (result.1, config))
