// SPDX-FileCopyrightText: 2020 TQ Tezos
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
      ([%Michelson ({| { FAILWITH } |} : string * unit -> proposal)]
        ("PROPOSAL_NOT_EXIST", ()))

[@inline]
let ensure_voting_period_is_not_over (proposal, store : proposal * storage): storage =
  if Tezos.now >= proposal.start_date + int(store.voting_period)
    then ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
          ("VOTING_PERIOD_OVER", ()) : storage)
    else store

[@inline]
let ensure_proposal_is_unique (propose_params, store : propose_params * storage): proposal_key =
  let proposal_key = to_proposal_key(propose_params, Tezos.sender) in
  if Map.mem proposal_key store.proposals
    then ([%Michelson ({| { FAILWITH } |} : string * unit -> proposal_key)]
        ("PROPOSAL_NOT_UNIQUE", ()) : proposal_key)
    else proposal_key

// -----------------------------------------------------------------
// Propose
// -----------------------------------------------------------------

[@inline]
let check_is_proposal_valid (config, propose_params, store : config * propose_params * storage): storage =
  if config.proposal_check (propose_params, store)
    then store
    else ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
          ("FAIL_PROPOSAL_CHECK", ()) : storage)

[@inline]
let check_proposer_unfrozen_token (propose_params, ledger : propose_params * ledger): ledger =
  match Map.find_opt (Tezos.sender, unfrozen_token_id) ledger with
    None ->
      ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> ledger)]
        ("FA2_INSUFFICIENT_BALANCE", (propose_params.frozen_token, 0n)) : ledger)
  | Some current_balance ->
    if propose_params.frozen_token > current_balance
      then ([%Michelson ({| { FAILWITH } |} : string * unit -> ledger)]
            ("PROPOSAL_INSUFFICIENT_BALANCE", ()) : ledger)
      else ledger

[@inline]
let check_proposal_limit_reached (config, propose_params, store : config * propose_params * storage): storage =
  if config.max_proposals <= List.length store.proposal_key_list_sort_by_date
    then ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
          ("MAX_PROPOSALS_REACHED", ()) : storage)
    else store

let freeze (tokens, addr, ledger : nat * address * ledger): ledger =
  let ledger = debit_from (tokens, addr, unfrozen_token_id, ledger) in
  let ledger = credit_to (tokens, addr, frozen_token_id, ledger) in
  ledger

[@inline]
let unfreeze (tokens, addr, ledger : nat * address * ledger): ledger =
  let ledger = debit_from (tokens, addr, frozen_token_id, ledger) in
  let ledger = credit_to (tokens, addr, unfrozen_token_id, ledger) in
  ledger

let add_proposal (propose_params, store : propose_params * storage): storage =
  let proposal_key = ensure_proposal_is_unique (propose_params, store) in
  let timestamp = Tezos.now in
  let proposal : proposal =
    { upvotes = 0n
    ; downvotes = 0n
    ; start_date = timestamp
    ; metadata = propose_params.proposal_metadata
    ; proposer = Tezos.sender
    ; proposer_frozen_token = propose_params.frozen_token
    ; voters = ([] : (address * nat) list)
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
  let ledger = check_proposer_unfrozen_token (param, store.ledger) in

  let ledger = freeze (param.frozen_token, Tezos.sender, ledger) in
  let store = add_proposal (param, {store with ledger = ledger}) in
  ( ([] : operation list)
  , store
  )

// -----------------------------------------------------------------
// Vote
// -----------------------------------------------------------------

[@inline]
let check_voter_unfrozen_token (vote_param, author, store : vote_param * address * storage): storage =
  let current_balance =
    match Map.find_opt (author, unfrozen_token_id) store.ledger with
      None ->
        ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> nat)]
          ("FA2_INSUFFICIENT_BALANCE", (vote_param.vote_amount, 0n)) : nat)
    | Some value -> value
    in
  if vote_param.vote_amount > current_balance
    then ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
          ("VOTING_INSUFFICIENT_BALANCE", ()) : storage)
    else store

[@inline]
let submit_vote (proposal, vote_param, author, store : proposal * vote_param * address * storage): storage =
  let proposal_key = vote_param.proposal_key in

  let proposal =
    if vote_param.vote_type
      then { proposal with upvotes = proposal.upvotes + vote_param.vote_amount }
      else { proposal with downvotes = proposal.downvotes + vote_param.vote_amount }
    in
  let proposal =
    { proposal with
      voters = (author, vote_param.vote_amount) :: proposal.voters
    } in

  { store with
      ledger = freeze (vote_param.vote_amount, author, store.ledger)
    ; proposals = Map.add proposal_key proposal store.proposals
  }

[@inline]
let check_vote_limit_reached
    (config, proposal, vote_param : config * proposal * vote_param): vote_param =
  if config.max_votes < proposal.upvotes + proposal.downvotes + vote_param.vote_amount
    then ([%Michelson ({| { FAILWITH } |} : string * unit -> vote_param)]
          ("MAX_VOTES_REACHED", ()) : vote_param)
    else vote_param

let vote(votes, config, store : vote_param_permited list * config * storage): return =
  let accept_vote = fun (store, pp : storage * vote_param_permited) ->
    let (param, author, store) = verify_permit_protected_vote (pp, store) in
    let proposal = check_if_proposal_exist (param.proposal_key, store) in
    let vote_param = check_vote_limit_reached (config, proposal, param) in
    let store = ensure_voting_period_is_not_over (proposal, store) in
    let store = check_voter_unfrozen_token (param, author, store) in
    let store = submit_vote (proposal, param, author, store) in
    store
    in
  ( ([] : operation list)
  , List.fold accept_vote votes store
  )

// -----------------------------------------------------------------
// Admin entrypoints
// -----------------------------------------------------------------

// Update voting period of all ongoing and new proposals.
[@inline]
let set_voting_period(new_period, config, store : voting_period * config * storage): return =
  let store = authorize_admin store in
  let store =
    if   config.max_voting_period < new_period
      || config.min_voting_period > new_period
      then ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
            ("OUT_OF_BOUND_VOTING_PERIOD", ()) : storage)
      else store
    in
  let store = { store with voting_period = new_period } in
  (([] : operation list), store)

// Update quroum_threshold. The new quorum_threshold affects
// all ongoing and new proposals.
[@inline]
let set_quorum_threshold(new_threshold, config, store : quorum_threshold * config * storage): return =
  let store = authorize_admin store in
  let store =
    if   config.max_quorum_threshold < new_threshold
      || config.min_quorum_threshold > new_threshold
      then ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
            ("OUT_OF_BOUND_QUORUM_THRESHOLD", ()) : storage)
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
    match Map.find_opt (addr, frozen_token_id) store.ledger with
      None ->
        ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
          ("PROPOSER_NOT_EXIST_IN_LEDGER", ()) : nat)
    | Some value -> value
    in
  if unfreeze_value > actual_frozen_value
    then actual_frozen_value
    else unfreeze_value

[@inline]
let burn_frozen_token (tokens, addr, store : nat * address * storage): storage =
  {store with ledger = debit_from(tokens, addr, frozen_token_id, store.ledger)}

// Burn the "slash_amount" calculated by "config.rejected_proposal_return_value".
// See the Haskell version for details.
[@inline]
let burn_slash_amount (slash_amount, frozen_tokens, addr, store : nat * nat * address * storage): storage =
  let to_burn =
    match Michelson.is_nat(frozen_tokens - slash_amount) with
      Some value_ -> slash_amount
    | None -> frozen_tokens
    in
  burn_frozen_token (to_burn, addr, store)

let unfreeze_proposer_and_voter_token
  (rejected_proposal_return, is_accepted, proposal, proposal_key, store :
    (proposal * storage -> nat) * bool * proposal * proposal_key * storage): storage =
  // unfreeze_proposer_token
  let (tokens, store) =
    if is_accepted
    then (proposal.proposer_frozen_token, store)
    else
      let slash_amount = rejected_proposal_return (proposal, store) in
      let frozen_tokens = proposal.proposer_frozen_token in
      let store = burn_slash_amount(slash_amount, frozen_tokens, proposal.proposer, store) in
      let tokens =
            match Michelson.is_nat(frozen_tokens - slash_amount) with
              Some value -> value
            | None -> 0n
            in
      (tokens, store)
    in
  let tokens = check_balance_less_then_frozen_value
              (tokens, proposal.proposer, proposal, proposal_key, store) in
  let ledger = unfreeze(tokens, proposal.proposer, store.ledger) in
  // unfreeze_voter_token
  let do_unfreeze = fun (ledger, (addr, tokens) : ledger * (address * nat)) ->
    unfreeze(tokens, addr, ledger)
    in
  {store with ledger = List.fold do_unfreeze proposal.voters ledger}

[@inline]
let is_voting_period_over (proposal, store : proposal * storage): bool =
  Tezos.now >= proposal.start_date + int(store.voting_period)

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
    let store = unfreeze_proposer_and_voter_token (config.rejected_proposal_return_value, cond, proposal, proposal_key, store) in
    let (new_ops, store) =
      if cond
      then config.decision_lambda (proposal, store)
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
        ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
          ("BAD_ENTRYPOINT_PARAMETER", ()) : storage)
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
      let store = unfreeze_proposer_and_voter_token (config.rejected_proposal_return_value, true, proposal, proposal_key, store) in
      let store = delete_proposal (proposal.start_date, proposal_key, store) in
      (([] : operation list), store)
    else
      ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
        ("FAIL_DROP_PROPOSAL_NOT_ACCEPTED", ()) : return)
  else
    ([%Michelson ({| { FAILWITH } |} : string * unit -> return)]
      ("FAIL_DROP_PROPOSAL_NOT_OVER", ()) : return)
