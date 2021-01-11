// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Proposal.hs module

#include "types.mligo"

let propose (param, store : propose_params * storage) : return =
  not_implemented("propose")

let drop_proposal (param, store : proposal_key * storage) : return =
  not_implemented("drop_proposal")

let vote(param, store : vote_param_permited list * storage) : return =
  not_implemented("vote")

let set_voting_period(param, store : voting_period * storage) : return =
  not_implemented("set_voting_period")

let set_quorum_threshold(param, store : voting_period * storage) : return =
  not_implemented("set_quorum_threshold")

let flush(param, config, store : nat * config * storage) : return =
  not_implemented("flush")
