// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"
#include "common.mligo"
#include "token.mligo"
#include "permit.mligo"
#include "proposal/freeze_history.mligo"
#include "proposal/quorum_threshold.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let to_proposal_key (propose_params: propose_params): proposal_key =
  Crypto.blake2b (Bytes.pack propose_params)

let fetch_proposal (proposal_key, store : proposal_key * storage): proposal =
  match Map.find_opt proposal_key store.proposals with
  | Some p -> p
  | None -> (failwith("PROPOSAL_NOT_EXIST") : proposal)

[@inline]
let check_if_proposal_exist (proposal_key, store : proposal_key * storage): proposal =
  let p = fetch_proposal (proposal_key, store) in
  if Set.mem (p.start_level, proposal_key) store.proposal_key_list_sort_by_level
    then p
    else (failwith("PROPOSAL_NOT_EXIST") : proposal)

// Gets the current stage counting how many `period` s have passed since
// the 'start_level`. The stages are zero-index.
let get_current_stage_num(start, vp : blocks * period) : nat =
  match is_nat((Tezos.level - start.blocks) : int) with
  | Some (elapsed_levels) -> elapsed_levels/vp.blocks
  | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
      ("BAD_STATE", ()))

[@inline]
let ensure_proposal_voting_stage (proposal, period, store : proposal * period * storage): storage =
  let current_stage = get_current_stage_num(store.start_level, period) in
  if current_stage = proposal.voting_stage_num
  then store
  else (failwith("VOTING_STAGE_OVER") : storage)

// Checks that a given stage number is a proposing stage
// Only odd stage numbers are proposing stages, in which a proposal can be
// submitted.
let ensure_proposing_stage(stage_num, store : nat * storage): storage =
  if (stage_num mod 2n) = 1n
  then store
  else (failwith("NOT_PROPOSING_STAGE") : storage)

[@inline]
let ensure_proposal_is_unique (propose_params, store : propose_params * storage): proposal_key =
  let proposal_key = to_proposal_key(propose_params) in
  if Map.mem proposal_key store.proposals
    then (failwith("PROPOSAL_NOT_UNIQUE") : proposal_key)
    else proposal_key

// -----------------------------------------------------------------
// Delegate
// -----------------------------------------------------------------

// Check if the `author`/`sender` address is the same as `from` or a delegate of `from`.
// Return `from` as the result.
[@inline]
let check_delegate (from, author, store : address * address * storage): address =
  let key: delegate = { owner = from; delegate = author } in
  if (author <> from) && not (Big_map.mem key store.delegates) then
    (failwith("NOT_DELEGATE") : address)
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

// -----------------------------------------------------------------
// Propose
// -----------------------------------------------------------------

[@inline]
let check_proposal_limit_reached (config, store : config * storage): storage =
  if config.max_proposals <= List.length store.proposal_key_list_sort_by_level
  then (failwith("MAX_PROPOSALS_REACHED") : storage)
  else store

let lock_governance_tokens (tokens, addr, frozen_total_supply, governance_token : nat * address * nat * governance_token)
    : (operation list * nat) =
  // Call transfer on token_contract to transfer `token` number of
  // tokens from `addr` to the address of this contract.
  let param = { from_ = addr; txs = [{ amount = tokens; to_ = Tezos.self_address; token_id = governance_token.token_id }]} in
  let operation = make_transfer_on_token ([param], governance_token.address) in
  ([operation], frozen_total_supply + tokens)

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
      else ([%Michelson ({| { FAILWITH } |} : (string * unit) -> freeze_history)]
              ("NOT_ENOUGH_FROZEN_TOKENS", ()) : freeze_history)
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
        (failwith("BAD_STATE") : nat)
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
    ; voters = (Map.empty : voter_map)
    ; quorum_threshold = store.quorum_threshold_at_cycle.quorum_threshold
    } in
  { store with
    proposals =
      Map.add proposal_key proposal store.proposals
  ; proposal_key_list_sort_by_level =
      Set.add ({blocks = Tezos.level}, proposal_key) store.proposal_key_list_sort_by_level
  }

// -----------------------------------------------------------------
// Vote
// -----------------------------------------------------------------

let submit_vote (proposal, vote_param, author, period, store : proposal * vote_param * address * period * storage): storage =
  let proposal_key = vote_param.proposal_key in
  let proposal =
        let map_key = (author, vote_param.vote_type) in
        let new_votes = match Map.find_opt map_key proposal.voters with
          | Some votes -> votes + vote_param.vote_amount
          | None -> vote_param.vote_amount in
        { proposal with voters = Map.add map_key new_votes proposal.voters } in
  let proposal =
        if vote_param.vote_type
          then { proposal with upvotes = proposal.upvotes + vote_param.vote_amount }
          else { proposal with downvotes = proposal.downvotes + vote_param.vote_amount } in
  let store = stake_tk(vote_param.vote_amount, author, period, store) in
  { store with proposals = Map.add proposal_key proposal store.proposals }

[@inline]
let check_vote_limit_reached
    (config, proposal, vote_param : config * proposal * vote_param): vote_param =
  if config.max_voters <= Map.size(proposal.voters)
      // We use <= because this function is called before the voter is added to the proposal.voters
      // map.
  then (failwith("MAX_VOTERS_REACHED") : vote_param)
  else vote_param

let vote(votes, config, store : vote_param_permited list * config * storage): return =
  let accept_vote = fun (store, pp : storage * vote_param_permited) ->
    let (param, author, store) = verify_permit_protected_vote (pp, store) in
    let valid_from = check_delegate (pp.argument.from, author, store) in
    let proposal = check_if_proposal_exist (param.proposal_key, store) in
    let vote_param = check_vote_limit_reached (config, proposal, param) in
    let store = ensure_proposal_voting_stage (proposal, config.period, store) in
    let store = submit_vote (proposal, vote_param, valid_from, config.period, store) in
    store
  in
  (nil_op, List.fold accept_vote votes store)


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
          | None -> (failwith("BAD_STATE") : nat) in
        { store with
            freeze_history = new_freeze_history
          ; frozen_total_supply = new_total_supply
        }
    | None -> (failwith("BAD_STATE") : storage)

let unfreeze_proposer_and_voter_token
  (rejected_proposal_slash, is_accepted, proposal, period, fixed_fee, store :
    (proposal * contract_extra -> nat) * bool * proposal * period * nat * storage): storage =
  // unfreeze_proposer_token
  let (tokens, burn_amount) =
    if is_accepted
    then (proposal.proposer_frozen_token + fixed_fee, 0n)
    else
      let slash_amount = rejected_proposal_slash (proposal, store.extra) in
      let frozen_tokens = proposal.proposer_frozen_token + fixed_fee in
      let desired_burn_amount = slash_amount + fixed_fee in
      let tokens =
            match Michelson.is_nat(frozen_tokens - desired_burn_amount) with
              Some value -> value
            | None -> 0n
            in
      (tokens, desired_burn_amount)
    in
  let store = unstake_tk(tokens, burn_amount, proposal.proposer, period, store) in

  // unfreeze_voter_token
  let do_unfreeze = fun
        ( store, voter_with_vote : storage * ((address * vote_type) * nat))
          -> unstake_tk(voter_with_vote.1, 0n, voter_with_vote.0.0, period, store) in

  Map.fold do_unfreeze proposal.voters store

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

// Delete a proposal from 'proposal_key_list_sort_by_level'
[@inline]
let delete_proposal
    (level, proposal_key, store : blocks * proposal_key * storage): storage =
  { store with proposal_key_list_sort_by_level =
    Set.remove (level, proposal_key) store.proposal_key_list_sort_by_level
  }

let propose (param, config, store : propose_params * config * storage): return =
  let valid_from = check_delegate (param.from, Tezos.sender, store) in
  let _ : unit = config.proposal_check (param, store.extra) in
  let store = check_proposal_limit_reached (config, store) in
  let amount_to_freeze = param.frozen_token + config.fixed_proposal_fee_in_token in
  let current_stage = get_current_stage_num(store.start_level, config.period) in
  let store = update_quorum(current_stage, store, config) in
  let store = stake_tk(amount_to_freeze, valid_from, config.period, store) in
  let store = add_proposal (param, config.period, store) in
  (nil_op, store)

[@inline]
let handle_proposal_is_over
    (config, start_level, proposal_key, store, ops, counter
      : config * blocks * proposal_key * storage * operation list * counter
    )
    : (operation list * storage * counter) =
  let proposal = fetch_proposal (proposal_key, store) in

  if is_proposal_age (proposal, config.proposal_expired_level)
  then (failwith("EXPIRED_PROPOSAL") : (operation list * storage * counter))
  else if is_proposal_age (proposal, config.proposal_flush_level)
       && counter.current < counter.total // not finished
  then
    let counter = { counter with current = counter.current + 1n } in
    let cond =    do_total_vote_meet_quorum_threshold(proposal, store)
              && proposal.upvotes > proposal.downvotes
    in
    let store = unfreeze_proposer_and_voter_token
          (config.rejected_proposal_slash_value, cond, proposal, config.period, config.fixed_proposal_fee_in_token, store) in
    let (new_ops, store) =
      if cond
      then
        let dl_out = config.decision_lambda { proposal = proposal; extras = store.extra } in
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
    let store = delete_proposal (start_level, proposal_key, store) in
    (ops, store, counter)
  else (ops, store, counter)

// Flush all proposals that passed their voting stage.
let flush(n, config, store : nat * config * storage): return =
  if n = 0n
  then (failwith("BAD_ENTRYPOINT_PARAMETER") : return)
  else
    let counter : counter = { current = 0n; total = n } in
    let flush_one
        (acc, e: (operation list * storage * counter) * (blocks * proposal_key)) =
          let (ops, store, counter) = acc in
          let (start_level, proposal_key) = e in
          handle_proposal_is_over (config, start_level, proposal_key, store, ops, counter)
        in
    let (ops, store, counter) =
      Set.fold flush_one store.proposal_key_list_sort_by_level (nil_op, store, counter)
    in
    // prevent empty flushes to avoid gas costs when unnecessary.
    if counter.current = 0n
    then (failwith("EMPTY_FLUSH") : return)
    else (ops, store)

// Removes an accepted and finished proposal by key.
let drop_proposal (proposal_key, config, store : proposal_key * config * storage): return =
  let proposal = check_if_proposal_exist (proposal_key, store) in
  let proposal_is_expired = is_proposal_age (proposal, config.proposal_expired_level) in

  if   (sender = proposal.proposer)
    || (sender = store.guardian && sender <> source) // Guardian cannot be equal to SOURCE
    || proposal_is_expired
  then
    let store = unfreeze_proposer_and_voter_token
          ( config.rejected_proposal_slash_value
          , false // A dropped proposal is treated as rejected regardless of its actual votes
          , proposal
          , config.period
          , config.fixed_proposal_fee_in_token
          , store
          ) in
    let store = delete_proposal (proposal.start_level, proposal_key, store) in
    (nil_op, store)
  else
    (failwith("DROP_PROPOSAL_CONDITION_NOT_MET") : return)

let freeze (amt, config, store : freeze_param * config * storage) : return =
  let addr = Tezos.sender in
  let (operations, frozen_total_supply) = lock_governance_tokens (amt, addr, store.frozen_total_supply, store.governance_token) in

  // Add the `amt` to the current stage frozen token count of the freeze-history.
  let current_stage = get_current_stage_num(store.start_level, config.period) in
  let new_freeze_history_for_address = match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = update_fh(current_stage, fh) in
        add_frozen_fh(amt, fh)
    | None -> { current_stage_num = current_stage; staked = 0n; current_unstaked = amt; past_unstaked = 0n;}
  in
  ((operations : operation list), { store with
      frozen_total_supply = frozen_total_supply
    ; freeze_history = Big_map.update addr (Some(new_freeze_history_for_address)) store.freeze_history
  })

let unfreeze (amt, config, store : unfreeze_param * config * storage) : return =
  let addr = Tezos.sender in
  let current_stage = get_current_stage_num(store.start_level, config.period) in

  let new_freeze_history =
    match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = update_fh(current_stage, fh) in
        let fh = sub_frozen_fh(amt, fh) in
        Big_map.update addr (Some(fh)) store.freeze_history
    | None ->
        ([%Michelson ({| { FAILWITH } |} : (string * unit) -> freeze_history)]
          ("NOT_ENOUGH_FROZEN_TOKENS", ()) : freeze_history)
  in

  let (operations, frozen_total_supply) = unlock_governance_tokens (amt, Tezos.sender, store.frozen_total_supply, store.governance_token) in

    ((operations : operation list), { store with
        freeze_history = new_freeze_history
      ; frozen_total_supply = frozen_total_supply
    })
