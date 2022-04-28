// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC
#if !PROPOSAL_H
#define PROPOSAL_H

#include "implementation.mligo"
#include "types.mligo"
#include "common.mligo"
#include "token.mligo"
#include "permit.mligo"
#include "error_codes.mligo"
#include "proposal/freeze_history.mligo"
#include "proposal/plist.mligo"
#include "proposal/helpers.mligo"
#include "proposal/quorum_threshold.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let ensure_proposal_is_unique (propose_params, store : propose_params * storage): proposal_key =
  let proposal_key = to_proposal_key(propose_params) in
  if Map.mem proposal_key store.proposals
    then (failwith proposal_not_unique: proposal_key)
    else proposal_key

let unstake_tk(token_amount, burn_amount, addr, period, store : nat * nat * address * period * storage): storage =
  let current_stage = get_current_stage_num(store.start_level, period) in
  match Big_map.find_opt addr store.freeze_history with
    | Some(fh) ->
        let fh = update_fh(current_stage, fh) in
        let fh = unstake_frozen_fh(token_amount, burn_amount, fh) in
        let new_freeze_history = Big_map.update addr (Some(fh)) store.freeze_history in
        let new_total_supply =
          match Michelson.is_nat (store.frozen_total_supply - burn_amount) with
            Some new_total_supply -> new_total_supply
          | None -> (failwith bad_state : nat) in
        { store with
            freeze_history = new_freeze_history
          ; frozen_total_supply = new_total_supply
        }
    | None -> (failwith bad_state : storage)

// -----------------------------------------------------------------
// Delegate
// -----------------------------------------------------------------

(*
 * Check if the `author`/`sender` address is the same as `from` or a delegate of `from`.
 * Return `from` as the result.
 *)
[@inline]
let check_delegate (from, author, store : address * address * storage): address =
  let key: delegate = { owner = from; delegate = author } in
  if (author <> from) && not (Big_map.mem key store.delegates) then
    (failwith not_delegate : address)
  else from

let update_delegate (delegates, param: delegates * update_delegate): delegates =
  let delegate_update =
    if param.enable
    then (Some unit)
    else (None : unit option) in
  let key: delegate =
      { owner = Tezos.sender
      ; delegate = param.delegate
      } in
  let updated_delegates = Big_map.update key delegate_update delegates
  in  updated_delegates

let update_delegates (params, store : update_delegate_params * storage): return =
  ( nil_op
  , { store with delegates = List.fold update_delegate params store.delegates }
  )


(*
 * Unstake voter's tokens on a proposal that has already been flushed or dropped.
 * Fail if the voter did not vote on that proposal, or voter has already unfreezed, or
 * the proposal was not yet flushed nor dropped.
 *)
let unstake_vote_one (config: config) (store , proposal_key : storage * proposal_key): storage =

  // Ensure proposal is already flushed or dropped.
  let _ = if plist_mem (proposal_key, store.ongoing_proposals_dlist)
            then (failwith unstake_invalid_proposal : unit)
            else unit in

  // Check if voter exist.
  let staked_vote_amount = match Big_map.find_opt (Tezos.sender, proposal_key) store.staked_votes with
      | Some v -> v
      | None -> (failwith voter_does_not_exist : staked_vote) in

  // Do the unstake
  let store = unstake_tk(staked_vote_amount, 0n, Tezos.sender, config.period, store) in

  // Remove voter's vote from staked amounts
  { store with
      staked_votes = Big_map.remove (Tezos.sender, proposal_key) store.staked_votes
  }

(*
 * Unstake voter's tokens on multiple proposals. Fail if an error occurred in one of the calls.
 *)
let unstake_vote (params, store : unstake_vote_param * storage): return =
  ( nil_op
  , List.fold (unstake_vote_one store.config) params store
  )

// -----------------------------------------------------------------
// Propose
// -----------------------------------------------------------------


let stake_tk(token_amount, addr, period, store : nat * address * period * storage): storage =
  let current_stage = get_current_stage_num(store.start_level, period) in
  let new_cycle_staked = store.quorum_threshold_at_cycle.staked + token_amount in
  let new_freeze_history = match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = update_fh(current_stage, fh) in
        let fh = stake_frozen_fh(token_amount, fh) in
        Big_map.update addr (Some(fh)) store.freeze_history
    | None ->
      if token_amount = 0n
      then store.freeze_history
      else (failwith not_enough_frozen_tokens : freeze_history)
  in { store with freeze_history = new_freeze_history; quorum_threshold_at_cycle = {store.quorum_threshold_at_cycle with staked = new_cycle_staked } }

[@inline]
let unlock_governance_tokens (tokens, addr, frozen_total_supply, governance_token : nat * address * nat * governance_token): (operation list * nat) =
  // Call transfer on token_contract to transfer `token` number of
  // tokens from `addr` to the address of this contract.
  let param = { from_ = Tezos.self_address; txs = [{ amount = tokens; to_ = addr; token_id = governance_token.token_id }]} in
  let operation = make_transfer_on_token ([param], governance_token.address) in
  let new_total_supply =
    match Michelson.is_nat (frozen_total_supply - tokens) with
      Some new_total_supply -> new_total_supply
    | None ->
        (failwith bad_state : nat)
  in ([operation], new_total_supply)

let add_proposal (propose_params, period, store : propose_params * period * storage): storage =
  let proposal_key = ensure_proposal_is_unique (propose_params, store) in
  let current_stage = get_current_stage_num(store.start_level, period) in
  let store = ensure_proposing_stage(current_stage, store) in
  let proposal : proposal =
    { upvotes = 0n
    ; downvotes = 0n
    ; start_level = {blocks = Tezos.level}
    ; voting_stage_num = current_stage + 1n
    ; metadata = propose_params.proposal_metadata
    ; proposer = propose_params.from
    ; proposer_frozen_token = propose_params.frozen_token
    ; quorum_threshold = store.quorum_threshold_at_cycle.quorum_threshold
    } in
  { store with
    proposals =
      Map.add proposal_key proposal store.proposals
  ; ongoing_proposals_dlist =
      plist_insert (proposal_key, store.ongoing_proposals_dlist)
  }

// -----------------------------------------------------------------
// Vote
// -----------------------------------------------------------------

let submit_vote (proposal, vote_param, author, period, store : proposal * vote_param * address * period * storage): storage =
  let proposal_key = vote_param.proposal_key in

  // Check if voter is already existed or not.
  let staked_vote = match Big_map.find_opt (author, proposal_key) store.staked_votes with
        | Some v -> (v : staked_vote)
        | None -> 0n in

  // Update staked vote amount
  let new_staked_vote = staked_vote + vote_param.vote_amount in

  let proposal =
        if vote_param.vote_type
          then { proposal with upvotes = proposal.upvotes + vote_param.vote_amount }
          else { proposal with downvotes = proposal.downvotes + vote_param.vote_amount } in

  let store = stake_tk(vote_param.vote_amount, author, period, store) in

  { store with
      proposals = Big_map.add proposal_key proposal store.proposals
  ;   staked_votes = Big_map.add (author, proposal_key) new_staked_vote store.staked_votes
  }


let vote(votes, store : vote_param_permited list * storage): return =
  let accept_vote = fun (store, pp : storage * vote_param_permited) ->
    let (param, author, store) = verify_permit_protected_vote (pp, store) in
    let valid_from = check_delegate (pp.argument.from, author, store) in
    let proposal = check_if_proposal_exist (param.proposal_key, store) in
    let store = ensure_proposal_voting_stage (proposal, store.config.period, store) in
    let store = submit_vote (proposal, param, valid_from, store.config.period, store) in
    store
  in
  (nil_op, List.fold accept_vote votes store)

let unstake_proposer_token
  (is_accepted, proposal, period, fixed_fee, store :
    bool * proposal * period * nat * storage): storage =
  // Get proposer token and burn amount
  let (tokens, burn_amount) =
    if is_accepted
    then (proposal.proposer_frozen_token + fixed_fee, 0n)
    else
      let slash_amount = rejected_proposal_slash_value (proposal, store.extra) in
      let frozen_tokens = proposal.proposer_frozen_token + fixed_fee in
      let desired_burn_amount = slash_amount + fixed_fee in
      let tokens =
            match Michelson.is_nat(frozen_tokens - desired_burn_amount) with
              Some value -> value
            | None -> 0n
            in
      (tokens, desired_burn_amount)
    in

  // Do the unstake for the proposer
  unstake_tk(tokens, burn_amount, proposal.proposer, period, store)

[@inline]
let is_proposal_age (proposal, target : proposal * blocks): bool =
  Tezos.level >= proposal.start_level.blocks + target.blocks

[@inline]
let do_total_vote_meet_quorum_threshold (proposal, store: proposal * storage): bool =
  let votes_placed = proposal.upvotes + proposal.downvotes in
  let total_supply = store.frozen_total_supply in
  // Note: this is equivalent to checking that the number of votes placed is
  // bigger or equal than the total supply of frozen tokens multiplied by the
  // quorum_threshold proportion.
  let reached_quorum = (votes_placed * quorum_denominator) / total_supply in
  (reached_quorum >= proposal.quorum_threshold.numerator)

let propose (param, store : propose_params * storage): return =
  let config = store.config in
  let valid_from = check_delegate (param.from, Tezos.sender, store) in
  let _ : unit = proposal_check (param, store.extra) in
  let amount_to_freeze = param.frozen_token + config.fixed_proposal_fee_in_token in
  let current_stage = get_current_stage_num(store.start_level, config.period) in
  let store = update_quorum(current_stage, store, config) in
  let store = stake_tk(amount_to_freeze, valid_from, config.period, store) in
  let store = add_proposal (param, config.period, store) in
  (nil_op, store)

(*
 * Unstake proposer token and execute decision callback based on if the proposal is passed or not.
 *)
[@inline]
let handle_proposal_is_over
    (config, proposal, store, ops
      : config * proposal * storage * operation list
    )
    : (operation list * storage) =
  let cond = do_total_vote_meet_quorum_threshold (proposal, store)
          && proposal.upvotes > proposal.downvotes
  in
  let store = unstake_proposer_token
        (cond, proposal, config.period, config.fixed_proposal_fee_in_token, store) in
  let (new_ops, store) =
    if cond
    then
      let dl_out = decision_callback { proposal = proposal; extras = store.extra } in
      let guardian = match dl_out.guardian with
        | Some g -> g
        | None -> store.guardian
      in (dl_out.operations,
            { store with extra = dl_out.extras
            ; guardian = guardian
            })
    else (nil_op, store)
  in
  let cons = fun (l, e : operation list * operation) -> e :: l in
  let ops = List.fold cons ops new_ops in
  (ops, store)


(*
 * Flush a proposal, starting from the first. Remove it when the flush is done, and move to the next proposal.
 *)
let rec flush_each (n, store, ops : int * storage * operation list): (operation list * storage * int) =
  let (plist_head, plist_new) = plist_pop store.ongoing_proposals_dlist in
  // Ensure plist is not empty.
  match plist_head with
    | Some first_key ->
        let proposal = check_if_proposal_exist (first_key, store) in
        // Ensure the proposal is not expired.
        let _ = if is_proposal_age (proposal, store.config.proposal_expired_level)
            then (failwith expired_proposal : unit)
            else unit in

        // Ensure counter is not reached yet and the the proposal is flushable.
        if (n > 0 && is_proposal_age (proposal, store.config.proposal_flush_level)) then
          // Unstake tokens, and execute proposal's decision_callback.
          let (ops, store) = handle_proposal_is_over (store.config, proposal, store, ops) in
          // Update the stored plist.
          let store = { store with ongoing_proposals_dlist = plist_new } in
          // Continue to the next proposal
          flush_each (n - 1, store, ops)
        else
          (ops, store, n)
    | None ->
        (ops, store, n)

(*
 * Flush all proposals that passed their voting stage.
 *)
let flush(n, store : nat * storage): return =
  let (ops, store, new_n) = flush_each (int(n), store, nil_op) in

  // prevent empty flushes to avoid gas costs when unnecessary.
  if new_n = int(n)
  then (failwith empty_flush : return)
  else (ops, store)

(*
 * Removes an accepted and finished proposal by key.
 *)
let drop_proposal (proposal_key, store : proposal_key * storage): return =
  let proposal = check_if_proposal_exist (proposal_key, store) in
  let proposal_is_expired = is_proposal_age (proposal, store.config.proposal_expired_level) in

  if   (sender = proposal.proposer)
    || (sender = store.guardian && sender <> source) // Guardian cannot be equal to SOURCE
    || proposal_is_expired
  then
    let store = unstake_proposer_token
          ( false // A dropped proposal is treated as rejected regardless of its actual votes
          , proposal
          , store.config.period
          , store.config.fixed_proposal_fee_in_token
          , store
          ) in
    let store =
          { store with ongoing_proposals_dlist =
              plist_delete (proposal_key, store.ongoing_proposals_dlist)
          } in
    (nil_op, store)
  else
    (failwith drop_proposal_condition_not_met : return)

let freeze (keyhash, store : freeze_param * storage) : return =
  // Procedure
  // 1. Check frozen_history and see if there are frozen tokens for current cycle, if yes, fail.
  // 2. Else query voting power for the sender, and save it it frozen history with current cycle.
  let addr = Tezos.sender in
  let current_cycle = get_current_cycle_num(storage.start_level, storage.config.voting_period_params.voting_stage_size + storage.config.voting_period_params.proposal_stage_size) in
  let amt = Tezos.voting_power (keyhash) in
  let nfh =
    match Big_map.find_opt addr store.freeze_history with
    | Some fh -> if fh.current_cycle_num == current_cycle_num
        then store.freeze_history
        else Big_map.update addr (Some { current_cycle_num = current_cycle ; frozen_tokens = amt }) store.freeze_history
    | None -> Big_map.add addr (Some { current_cycle_num = current_cycle ; frozen_tokens = amt }) store.freeze_history
  in
  (([] : operation list), { store with freeze_history = nfh })

#endif
