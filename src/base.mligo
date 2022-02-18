// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !BASE_H
#define BASE_H

// ID of an FA2 token
type token_id = nat

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

// FA2 transfer types
type transfer_destination =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }
type transfer_item =
  [@layout:comb]
  { from_ : address
  ; txs : transfer_destination list
  }
type transfer_params = transfer_item list

// -- Helpers -- //

// Internal helper to fold up to a number
type counter =
  { current : nat
  ; total : nat
  }

// -- DAO base types -- //

type nonce = nat

// Represents whether a voter has voted against (false) or for (true) a given proposal.
type vote_type = bool

type staked_vote = nat

// Amount of blocks.
type blocks = { blocks : nat }

// Length of a stage, in number of blocks
type period = blocks

// Representation of a quorum fraction. For efficiency, we only keep a `nat`
// for the numerator, whereas the denominator is not stored and has a fixed value
// of `quorum_denominator`.
type quorum_fraction = { numerator : int }
let quorum_denominator = 1000000n

// For safety, this is a version of quorum_fraction type
// that does not allow negative values.
type unsigned_quorum_fraction = { numerator : nat }

[@inline]
let to_signed(n : unsigned_quorum_fraction): quorum_fraction
  = { numerator = int(n.numerator) }

// Quorum threshold that a proposal needs to meet in order to be accepted,
// expressed as a fraction of the total_supply of frozen tokens, only
// storing the numerator while denominator is assumed to be
// `quorum_denominator`.
type quorum_threshold = unsigned_quorum_fraction

// Types to store info of a proposal
type proposal_key = bytes
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

// TZIP-17 permit data
type permit =
  { key : key
  ; signature : signature
  }

// TZIP-16 metadata map
type metadata_map = (string, bytes) big_map

// Instantiation-specific stored data
type contract_extra = (string, bytes) big_map

// -- Storage -- //

// External FA2 token used for governance
type governance_token =
  { address : address
  ; token_id : token_id
  }

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

// -- Parameter -- //

type freeze_param = nat
type unfreeze_param = nat
type unstake_vote_param = proposal_key list

type transfer_ownership_param = address

type propose_params =
  [@layout:comb]
  { from : address
  ; frozen_token : nat
  ; proposal_metadata : proposal_metadata
  }

type vote_param =
  { from : address
  ; proposal_key : proposal_key
  ; vote_type : vote_type
  ; vote_amount : nat
  }
type vote_param_permited =
  { argument : vote_param
  ; permit : permit option
  }

type burn_param =
  [@layout:comb]
  { from_ : address
  ; token_id : token_id
  ; amount : nat
  }
type mint_param =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }

type transfer_contract_tokens_param =
  { contract_address : address
  ; params : transfer_params
  }

type update_delegate =
  [@layout:comb]
  { enable : bool
  ; delegate : address
  }

type update_delegate_params = update_delegate list

(*
 * Entrypoints that forbids Tz transfers
 *)
type forbid_xtz_params =
  | Drop_proposal of proposal_key
  | Vote of vote_param_permited list
  | Flush of nat
  | Freeze of freeze_param
  | Unfreeze of unfreeze_param
  | Update_delegate of update_delegate_params
  | Unstake_vote of unstake_vote_param

(*
 * Entrypoints that allow Tz transfers
 *)
type allow_xtz_params_contract =
  | Propose of propose_params
  | Transfer_contract_tokens of transfer_contract_tokens_param
  | Transfer_ownership of transfer_ownership_param
  | Accept_ownership of unit
  | Default of unit

type decision_lambda_input =
  { proposal : proposal
  ; extras : contract_extra
  }

type decision_lambda_output =
  { operations : operation list
  ; extras : contract_extra
  ; guardian : address option
  }

// -- Config -- //

type freeze_history_list = (address * nat) list

type initial_config_data =
  { max_quorum : quorum_threshold
  ; min_quorum : quorum_threshold
  ; quorum_threshold : quorum_threshold
  ; period : period
  ; proposal_flush_level: blocks
  ; proposal_expired_level: blocks
  ; fixed_proposal_fee_in_token: nat
  ; max_quorum_change : unsigned_quorum_fraction
  ; quorum_change : unsigned_quorum_fraction
  ; governance_total_supply : nat
  }

type initial_storage_data =
  { admin : address
  ; guardian : address
  ; governance_token : governance_token
  ; start_level : blocks
  ; metadata_map : metadata_map
  ; freeze_history : freeze_history_list
  }

type initial_data =
  { storage_data : initial_storage_data
  ; config_data : initial_config_data
  }

type decision_lambda = decision_lambda_input -> decision_lambda_output

type config =
  { max_proposals : nat
  // ^ Determine the maximum number of ongoing proposals that are allowed in the contract.
  ; max_quorum_threshold : quorum_fraction
  // ^ Determine the maximum value of quorum threshold that is allowed.
  ; min_quorum_threshold : quorum_fraction
  // ^ Determine the minimum value of quorum threshold that is allowed.

  ; period : period
  // ^ Determines the stages length.

  ; fixed_proposal_fee_in_token : nat
  // ^ A base fee paid for submitting a new proposal.

  ; max_quorum_change : quorum_fraction
  // ^ A percentage value that limits the quorum_threshold change during
  // every update of the same.
  ; quorum_change : quorum_fraction
  // ^ A percentage value that is used in the computation of new quorum
  // threshold value.
  ; governance_total_supply : nat
  // ^ The total supply of governance tokens used in the computation of
  // of new quorum threshold value at each stage.

  ; proposal_flush_level : blocks
  // ^ The proposal age at (and above) which the proposal is considered flushable.
  // Has to be bigger than `period * 2`
  ; proposal_expired_level : blocks
  // ^ The proposal age at (and above) which the proposal is considered expired.
  // Has to be bigger than `proposal_flush_time`
  }

// -- Misc -- //

let nil_op = ([] : operation list)

#endif  // BASE_H included
