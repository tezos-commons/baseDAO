// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "plist.mligo"

let to_proposal_key (propose_params: propose_params): proposal_key =
  Crypto.blake2b (Bytes.pack propose_params)

let fetch_proposal (proposal_key, store : proposal_key * storage): proposal =
  match Map.find_opt proposal_key store.proposals with
  | Some p -> p
  | None -> (failwith proposal_not_exist : proposal)

[@inline]
let check_if_proposal_exist (proposal_key, store : proposal_key * storage): proposal =
  let p = fetch_proposal (proposal_key, store) in
  if plist_mem (proposal_key, store.ongoing_proposals_dlist)
    then p
    else (failwith proposal_not_exist : proposal)

(*
 * Gets the current cycle counting how many `cycles` s have passed since
 * the `start`. The stages are zero-index. Returns the cycle count and any
 * reminder levels
 *)
let get_current_cycle_num(start, cycle_size : blocks * blocks) : (nat * nat) =
  match is_nat(Tezos.level - start.blocks) with
  | Some (elapsed_levels) -> (match (ediv elapsed_levels cycle_size.blocks) with
      | (Some (q, r)) -> (q, r)
      | None -> (failwith bad_state : (nat * nat)))
  | None -> (failwith bad_state : (nat * nat))

let get_current_stage_num(start, vpp : blocks * voting_period_params) : nat = start.blocks

[@inline]
let ensure_proposal_voting_stage (proposal, vpp, store : proposal * voting_period_params * storage): storage =
  let current_stage = get_current_stage_num(store.start_level, vpp) in
  if current_stage = proposal.voting_stage_num
  then store
  else (failwith voting_stage_over : storage)

(*
 * Checks that a given stage number is a proposing stage
 * Only odd stage numbers are proposing stages, in which a proposal can be
 * submitted.
 *)
let ensure_proposing_stage(stage_num, store : nat * storage): storage =
  if (stage_num mod 2n) = 1n
  then store
  else (failwith not_proposing_stage : storage)
