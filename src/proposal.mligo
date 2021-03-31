// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Proposal.hs module

#include "types.mligo"
#include "common.mligo"
#include "token/fa2.mligo"
#include "permit.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let to_proposal_key (propose_params, sender_addr : propose_params * address): proposal_key =
  Crypto.blake2b (Bytes.pack (propose_params, sender_addr))

[@inline]
let check_if_proposal_exist (proposal_key, store : proposal_key * storage): proposal =
  match Map.find_opt proposal_key store.proposals with
    Some p -> p
  | None ->
      (failwith("PROPOSAL_NOT_EXIST") : proposal)

// Gets the current period counting how many `voting_period` s have passed since
// the 'started_on` timestamp. The periods start from zero index.
let get_current_period_num(last_period_change, vp_length : last_period_change * nat) : nat =
  match is_nat((Tezos.now - last_period_change.changed_on) : int) with
        | Some (elapsed_time) -> last_period_change.period_num + elapsed_time/vp_length
        | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
            ("STARTED_ON_IN_FUTURE", ()))

[@inline]
let ensure_proposal_voting_period (proposal, store : proposal * storage): storage =
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in
  if current_period = (proposal.period_num + 1n)
  then store
  else (failwith("VOTING_PERIOD_OVER") : storage)

// Checks that a given period number is a proposing period
// Only odd period numbers are proposing periods, in which a proposal can be
// submitted.
let ensure_proposing_period(period_num, store : nat * storage): storage =
  if (period_num mod 2n) = 0n
  then store
  else (failwith("NOT_PROPOSING_PERIOD") : storage)

[@inline]
let ensure_proposal_is_unique (propose_params, store : propose_params * storage): proposal_key =
  let proposal_key = to_proposal_key(propose_params, Tezos.sender) in
  if Map.mem proposal_key store.proposals
    then (failwith("PROPOSAL_NOT_UNIQUE") : proposal_key)
    else proposal_key

// -----------------------------------------------------------------
// Freeze history operations
// -----------------------------------------------------------------

let add_frozen_fh (amt, fh : nat * address_freeze_history)
    : address_freeze_history =
  { fh with current_unstaked = fh.current_unstaked + amt }

let sub_frozen_fh (amt, fh : nat * address_freeze_history)
    : address_freeze_history =
  match is_nat(fh.past_unstaked - amt) with
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ( "NOT_ENOUGH_FROZEN_TOKENS", ()) : address_freeze_history)
  | Some new_amt ->
      { fh with past_unstaked = new_amt }

let stake_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  let fh = sub_frozen_fh(amt, fh) in
  { fh with staked = fh.staked + amt }

let unstake_frozen_fh (amt, fh : nat * address_freeze_history)
    : address_freeze_history =
  match is_nat(fh.staked - amt) with
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ("NOT_ENOUGH_STAKED_TOKENS", ()) : address_freeze_history)
  | Some new_amt ->
      // Adding to past_unstaked should be fine since as of now, the staked tokens have to be from
      // past periods.
      { fh with staked = new_amt; past_unstaked = fh.past_unstaked + amt }

// Update a possibly outdated freeze_history for the current period
let update_fh (current_period, freeze_history : nat * address_freeze_history): address_freeze_history =
  if freeze_history.current_period_num < current_period
    then
      { current_period_num = current_period
      ; staked = freeze_history.staked
      ; current_unstaked = 0n
      ; past_unstaked = freeze_history.current_unstaked + freeze_history.past_unstaked
      }
    else freeze_history

// -----------------------------------------------------------------
// Propose
// -----------------------------------------------------------------

[@inline]
let check_is_proposal_valid (config, propose_params, store : config * propose_params * storage): storage =
  if config.proposal_check (propose_params, store.extra)
    then store
    else (failwith("FAIL_PROPOSAL_CHECK") : storage)

[@inline]
let check_proposal_limit_reached (config, propose_params, store : config * propose_params * storage): storage =
  if config.max_proposals <= List.length store.proposal_key_list_sort_by_date
    then (failwith("MAX_PROPOSALS_REACHED") : storage)
    else store

let freeze_on_ledger (tokens, addr, ledger, total_supply, unfrozen_token_id, frozen_token_id : nat * address * ledger * total_supply * token_id * token_id)
    : (ledger * total_supply) =
  let (ledger, total_supply) = debit_from (tokens, addr, unfrozen_token_id, ledger, total_supply) in
  let (ledger, total_supply) = credit_to (tokens, addr, frozen_token_id, ledger, total_supply) in
  (ledger, total_supply)


let stake_tk(token_amount, addr, store : nat * address * storage): storage =
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in
  let new_freeze_history = match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = update_fh(current_period, fh) in
        let fh = stake_frozen_fh(token_amount, fh) in
        Big_map.update addr (Some(fh)) store.freeze_history
    | None ->
      if token_amount = 0n
      then store.freeze_history
      else ([%Michelson ({| { FAILWITH } |} : (string * unit) -> freeze_history)]
              ("NOT_ENOUGH_FROZEN_TOKENS", ()) : freeze_history)
  in { store with freeze_history = new_freeze_history }

[@inline]
let unfreeze_on_ledger (tokens, addr, ledger, total_supply, unfrozen_token_id, frozen_token_id : nat * address * ledger * total_supply * token_id * token_id): (ledger * total_supply) =
  let (ledger, total_supply) = debit_from (tokens, addr, frozen_token_id, ledger, total_supply) in
  let (ledger, total_supply) = credit_to (tokens, addr, unfrozen_token_id, ledger, total_supply) in
  (ledger, total_supply)

let add_proposal (propose_params, store : propose_params * storage): storage =
  let proposal_key = ensure_proposal_is_unique (propose_params, store) in
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in
  let store = ensure_proposing_period(current_period, store) in
  let timestamp = Tezos.now in
  let proposal : proposal =
    { upvotes = 0n
    ; downvotes = 0n
    ; start_date = timestamp
    ; period_num = current_period
    ; metadata = propose_params.proposal_metadata
    ; proposer = Tezos.sender
    ; proposer_frozen_token = propose_params.frozen_token
    ; proposer_fixed_fee_in_token = store.fixed_proposal_fee_in_token
    ; voters = ([] : voter list)
    } in
  { store with
    proposals =
      Map.add proposal_key proposal store.proposals
  ; proposal_key_list_sort_by_date =
      Set.add (timestamp, proposal_key) store.proposal_key_list_sort_by_date
  }

let propose (param, config, store : propose_params * config * storage): return =
  let store = check_is_proposal_valid (config, param, store) in
  let store = check_proposal_limit_reached (config, param, store) in
  let amount_to_freeze = param.frozen_token + store.fixed_proposal_fee_in_token in
  let store = stake_tk(amount_to_freeze, Tezos.sender, store) in
  let store = add_proposal (param, store) in
  ( ([] : operation list)
  , store
  )

// -----------------------------------------------------------------
// Vote
// -----------------------------------------------------------------

let submit_vote (proposal, vote_param, author, store : proposal * vote_param * address * storage): storage =
  let proposal_key = vote_param.proposal_key in

  let proposal =
    if vote_param.vote_type
      then { proposal with upvotes = proposal.upvotes + vote_param.vote_amount }
      else { proposal with downvotes = proposal.downvotes + vote_param.vote_amount }
    in
  let voter =
    { voter_address = author
    ; vote_amount = vote_param.vote_amount
    ; vote_type = vote_param.vote_type
    } in
  let proposal =
    { proposal with
      voters = voter :: proposal.voters
    } in
  let store = stake_tk(vote_param.vote_amount, author, store) in
  { store with proposals = Map.add proposal_key proposal store.proposals }

[@inline]
let check_vote_limit_reached
    (config, proposal, vote_param : config * proposal * vote_param): vote_param =
  if config.max_votes < proposal.upvotes + proposal.downvotes + vote_param.vote_amount
    then (failwith("MAX_VOTES_REACHED") : vote_param)
    else vote_param

let vote(votes, config, store : vote_param_permited list * config * storage): return =
  let accept_vote = fun (store, pp : storage * vote_param_permited) ->
    let (param, author, store) = verify_permit_protected_vote (pp, store) in
    let proposal = check_if_proposal_exist (param.proposal_key, store) in
    let vote_param = check_vote_limit_reached (config, proposal, param) in
    let store = ensure_proposal_voting_period (proposal, store) in
    let store = submit_vote (proposal, param, author, store) in
    store
    in
  ( ([] : operation list)
  , List.fold accept_vote votes store
  )

// -----------------------------------------------------------------
// Admin entrypoints
// -----------------------------------------------------------------

// Update a fixed fee in the native token for submitting
// a proposal. The new fee affects only those proposals
// submitted after the call.
[@inline]
let set_fixed_fee_in_token(new_fee, store : nat * storage): return =
  let store = authorize_admin store in
  let store = { store with fixed_proposal_fee_in_token = new_fee } in
  (([] : operation list), store)

// Update voting period of all ongoing and new proposals.
[@inline]
let set_voting_period(new_period, config, store : voting_period * config * storage): return =
  let store = authorize_admin store in
  let current_vp = get_current_period_num(store.last_period_change, store.voting_period) in
  let vp_log : last_period_change =
    { changed_on = Tezos.now
    ; period_num = current_vp
    } in

  let store =
    if   config.max_voting_period < new_period
      || config.min_voting_period > new_period
      then (failwith("OUT_OF_BOUND_VOTING_PERIOD") : storage)
      else store
    in
  let store = { store with voting_period = new_period; last_period_change = vp_log } in
  (([] : operation list), store)

// Update quroum_threshold. The new quorum_threshold affects
// all ongoing and new proposals.
[@inline]
let set_quorum_threshold(new_threshold, config, store : quorum_threshold * config * storage): return =
  let store = authorize_admin store in
  let store =
    if   config.max_quorum_threshold < new_threshold
      || config.min_quorum_threshold > new_threshold
      then (failwith("OUT_OF_BOUND_QUORUM_THRESHOLD") : storage)
      else store
    in
  let store = { store with quorum_threshold = new_threshold } in
  (([] : operation list), store)

// Used in "flush". See the Haskell version for explanation.
[@inline]
let check_balance_less_then_frozen_value
    (unfreeze_value, addr, proposal, proposal_key, store
      : nat * address * proposal * proposal_key * storage): nat =
  let actual_frozen_value =
    match Map.find_opt (addr, store.frozen_token_id) store.ledger with
      None ->
        (failwith("PROPOSER_NOT_EXIST_IN_LEDGER") : nat)
    | Some value -> value
    in
  if unfreeze_value > actual_frozen_value
    then actual_frozen_value
    else unfreeze_value

[@inline]
let burn_frozen_token (tokens, addr, store : nat * address * storage): storage =
  let (ledger, total_supply) = debit_from(tokens, addr, store.frozen_token_id, store.ledger, store.total_supply)
  in {store with ledger = ledger; total_supply = total_supply}

// Burn up to desired_burn_amount of tokens. The desired burn amount comprises
// slash amount calculated by "config.rejected_proposal_return_value" and
// the fixed fee payed for the proposal. The case when the desired burn amount
// is larger than the available frozen tokens is possible because the contract
// administrator can transfer the proposer's frozen tokens.
[@inline]
let burn_what_possible (desired_burn_amount, frozen_tokens, addr, store : nat * nat * address * storage): storage =
  let to_burn =
    match Michelson.is_nat(frozen_tokens - desired_burn_amount) with
      Some value_ -> desired_burn_amount
    | None -> frozen_tokens
    in
  burn_frozen_token (to_burn, addr, store)

let unstake_tk(token_amount, addr, store : nat * address * storage): storage =
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in
  match Big_map.find_opt addr store.freeze_history with
    | Some(fh) ->
        let fh = update_fh(current_period, fh) in
        let fh = unstake_frozen_fh(token_amount, fh) in
        let new_freze_history = Big_map.update addr (Some(fh)) store.freeze_history in
        { store with freeze_history = new_freze_history }
    | None -> ([%Michelson ({| { FAILWITH } |} : (string * unit) -> storage)]
          ("NOT_ENOUGH_STAKED_TOKENS", ()) : storage)

let unfreeze_proposer_and_voter_token
  (rejected_proposal_return, is_accepted, proposal, proposal_key, store :
    (proposal * contract_extra -> nat) * bool * proposal * proposal_key * storage): storage =
  // unfreeze_proposer_token
  let (tokens, store) =
    if is_accepted
    then (proposal.proposer_frozen_token + proposal.proposer_fixed_fee_in_token, store)
    else
      let slash_amount = rejected_proposal_return (proposal, store.extra) in
      let fee = proposal.proposer_fixed_fee_in_token in
      let frozen_tokens = proposal.proposer_frozen_token + fee in
      let desired_burn_amount = slash_amount + fee in
      let store =
        burn_what_possible
          (desired_burn_amount, frozen_tokens, proposal.proposer, store) in
      let tokens =
            match Michelson.is_nat(frozen_tokens - desired_burn_amount) with
              Some value -> value
            | None -> 0n
            in
      (tokens, store)
    in
  let store = unstake_tk(proposal.proposer_frozen_token, proposal.proposer, store) in

  // unfreeze_voter_token
  let do_unfreeze = fun
        ( store, voter
        : storage * voter
        ) -> unstake_tk(voter.vote_amount, voter.voter_address, store) in

  List.fold do_unfreeze proposal.voters store

[@inline]
let is_voting_period_over (proposal, store : proposal * storage): bool =
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in
  current_period > proposal.period_num + 1n

[@inline]
let do_total_vote_meet_quorum_threshold (proposal, store : proposal * storage): bool =
  store.quorum_threshold <= proposal.upvotes + proposal.downvotes

// Delete a proposal from 'sProposalKeyListSortByDate'
[@inline]
let delete_proposal
    (start_date, proposal_key, store : timestamp * proposal_key * storage): storage =
  { store with proposal_key_list_sort_by_date =
    Set.remove (start_date, proposal_key) store.proposal_key_list_sort_by_date
  }

[@inline]
let handle_proposal_is_over
    (config, start_date, proposal_key, store, ops, counter
      : config * timestamp * proposal_key * storage * operation list * counter
    )
    : (operation list * storage * counter) =
  let proposal = check_if_proposal_exist (proposal_key, store) in
  if    is_voting_period_over (proposal, store)
     && counter.current < counter.total // not finished
  then
    let counter = { counter with current = counter.current + 1n } in
    let cond =    do_total_vote_meet_quorum_threshold(proposal, store)
               && proposal.upvotes > proposal.downvotes
    in
    let store = unfreeze_proposer_and_voter_token
          (config.rejected_proposal_return_value, cond, proposal, proposal_key, store) in
    let (new_ops, store) =
      if cond
      then
        let (ops, new_extra) = config.decision_lambda (proposal, store.extra)
        in (ops, { store with extra = new_extra })
      else (([] : operation list), store)
    in
    let cons = fun (l, e : operation list * operation) -> e :: l in
    let ops = List.fold cons ops new_ops in
    let store = delete_proposal (start_date, proposal_key, store) in
    (ops, store, counter)
  else (ops, store, counter)

// Flush all proposals that passed their voting period.
let flush(n, config, store : nat * config * storage): return =
  let store =
    if n = 0n
      then
        (failwith("BAD_ENTRYPOINT_PARAMETER") : storage)
      else store
    in

  let counter : counter =
    { current = 0n
    ; total = n
    } in
  let flush_one
      (acc, e: (operation list * storage * counter) * (timestamp * proposal_key)) =
        let (ops, store, counter) = acc in
        let (start_date, proposal_key) = e in
        handle_proposal_is_over (config, start_date, proposal_key, store, ops, counter)
      in
  let (ops, store, counter) =
    Set.fold flush_one store.proposal_key_list_sort_by_date (([] : operation list), store, counter) in
  (ops, store)

// Removes an accepted and finished proposal by key.
let drop_proposal (proposal_key, config, store : proposal_key * config * storage): return =
  let store = authorize_admin store in

  let proposal = check_if_proposal_exist (proposal_key, store) in
  if is_voting_period_over(proposal, store)
  then
    if   do_total_vote_meet_quorum_threshold(proposal, store)
      && proposal.upvotes > proposal.downvotes
    then
      let store = unfreeze_proposer_and_voter_token
            (config.rejected_proposal_return_value, true, proposal, proposal_key, store) in
      let store = delete_proposal (proposal.start_date, proposal_key, store) in
      (([] : operation list), store)
    else
      (failwith("FAIL_DROP_PROPOSAL_NOT_ACCEPTED") : return)
  else
    (failwith("FAIL_DROP_PROPOSAL_NOT_OVER") : return)


let freeze (amt, config, store : freeze_param * config * storage) : return =
  let addr = Tezos.sender in
  let (ledger, total_supply) = freeze_on_ledger (amt, addr, store.ledger, store.total_supply, store.unfrozen_token_id, store.frozen_token_id) in

  // Add the `amt` to the current period frozen token count of the freeze-history.
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in
  let new_freeze_history_for_address = match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = update_fh(current_period, fh) in
        add_frozen_fh(amt, fh)
    | None -> { current_period_num = current_period; staked = 0n; current_unstaked = amt; past_unstaked = 0n;}
  in
  (([] : operation list), { store with
      ledger = ledger
    ; total_supply = total_supply
    ; freeze_history = Big_map.update addr (Some(new_freeze_history_for_address)) store.freeze_history
  })

let unfreeze (amt, config, store : unfreeze_param * config * storage) : return =
  let addr = Tezos.sender in
  let current_period = get_current_period_num(store.last_period_change, store.voting_period) in

  let new_freeze_history =
    match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = update_fh(current_period, fh) in
        let fh = sub_frozen_fh(amt, fh) in
        Big_map.update addr (Some(fh)) store.freeze_history
    | None ->
        ([%Michelson ({| { FAILWITH } |} : (string * unit) -> freeze_history)]
          ("NOT_ENOUGH_FROZEN_TOKENS", ()) : freeze_history)
  in

  let (ledger, total_supply) = unfreeze_on_ledger (amt, Tezos.sender, store.ledger, store.total_supply, store.unfrozen_token_id, store.frozen_token_id) in

    (([] : operation list), { store with
        ledger = ledger
      ; total_supply = total_supply
      ; freeze_history = new_freeze_history
    })
