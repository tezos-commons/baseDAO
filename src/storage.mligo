// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !STORAGE_H
#define STORAGE_H

#include "implementation.mligo"

type proposal_key = bytes
type freeze_history_list = (address * nat) list
type token_id = nat

// Instantiation-specific stored data
type contract_extra = (string, bytes) big_map

// Amount of blocks.
type blocks = { blocks : nat }

// For safety, this is a version of quorum_fraction type
// that does not allow negative values.
type unsigned_quorum_fraction = { numerator : nat }

// External FA2 token used for governance
type governance_token =
  { address : address
  ; token_id : token_id
  }

type staked_vote = nat

// Frozen token history for an address.
// This tracks the stage number in which it was last updated and differentiates between
// tokens that were frozen during that stage and the ones frozen in any other before.
// It does so because only tokens that were frozen in the past can be staked, which is
// also why it tracks staked tokens in a single field.
type address_freeze_history =
  { current_stage_num : nat
  ; staked : nat
  ; current_unstaked : nat
  ; past_unstaked : nat
  }

// Frozen token history for all addresses
type freeze_history = (address, address_freeze_history) big_map

// TZIP-16 metadata map
type metadata_map = (string, bytes) big_map

type nonce = nat

// Quorum threshold that a proposal needs to meet in order to be accepted,
// expressed as a fraction of the total_supply of frozen tokens, only
// storing the numerator while denominator is assumed to be
// `quorum_denominator`.
type quorum_threshold = unsigned_quorum_fraction

// The way the staked token tracking work is as follows. The procedure to
// update quorum_threshold_at_cycle will reset the staked count to zero. And
// since this procedure is called from a `propose` call, which starts the
// staking of tokens for the cycle, and we increment `staked` count at each
// `propose` or `vote` call, the `staked` field will contain the tokens staked
// in that particular cycle. And the very first `propose` call in a cycle will
// see the staked tokens from the past cycle, and thus can use it to update the
// quorum threshold for the current cycle.
type quorum_threshold_at_cycle =
  { quorum_threshold : quorum_threshold
  ; last_updated_cycle : nat
  ; staked : nat
  }

// A `delegate` has the permission to `vote` and `propose` on behalf of an address
type delegate =
  [@layout:comb]
  { owner : address
  ; delegate : address
  }
type delegates = (delegate, unit) big_map

// Types to store info of a proposal
type proposal_metadata = bytes
type proposal =
  { upvotes : nat
  // ^ total amount of votes in favor
  ; downvotes : nat
  // ^ total amount of votes against
  ; start_level : blocks
  // ^ block level of submission, used to order proposals
  ; voting_stage_num : nat
  // ^ stage number in which it is possible to vote on this proposal
  ; metadata : proposal_metadata
  // ^ instantiation-specific additional data
  ; proposer : address
  // ^ address of the proposer
  ; proposer_frozen_token : nat
  // ^ amount of frozen tokens used by the proposer, exluding the fixed fee
  ; quorum_threshold: quorum_threshold
  // ^ quorum threshold at the cycle in which proposal was raised
  }

type storage =
  { governance_token : governance_token
  ; admin : address
  ; guardian : address // A special role that can drop any proposals at anytime
  ; pending_owner : address
  ; metadata : metadata_map
  ; extra : contract_extra
  ; proposals : (proposal_key, proposal) big_map
  ; proposal_key_list_sort_by_level : (blocks * proposal_key) set
  ; staked_votes : (address * proposal_key, staked_vote) big_map
  ; permits_counter : nonce
  ; freeze_history : freeze_history
  ; frozen_token_id : token_id
  ; start_level : blocks
  ; quorum_threshold_at_cycle : quorum_threshold_at_cycle
  ; frozen_total_supply : nat
  ; delegates : delegates
  }

#endif
