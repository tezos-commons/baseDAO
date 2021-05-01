// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !COMMON_H
#define COMMON_H

#include "types.mligo"

[@inline]
let authorize_admin (store : storage): storage =
  if sender = store.admin then
    store
  else
    (failwith("NOT_ADMIN") : storage)

#if VOTING_POWER_DAO
// gets the current (baking) cycle
let get_current_cycle(voting_period_params: voting_period_params) : nat =
  match is_nat((Tezos.level - voting_period_params.levels_per_cycle_change_at.1) : int) with
        | Some (elapsed_levels) -> voting_period_params.levels_per_cycle_change_at.0 + (elapsed_levels/voting_period_params.levels_per_cycle)
        | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
            ("STARTED_ON_IN_FUTURE", ()))

let get_current_period_num(voting_period_params: voting_period_params) : nat =
  match is_nat(get_current_cycle(voting_period_params) - voting_period_params.cycles_per_period_change_at.1) with
    | Some (elapsed_cycles) -> voting_period_params.cycles_per_period_change_at.0 + elapsed_cycles/voting_period_params.cycles_per_period
    | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
        ("STARTED_ON_IN_FUTURE", ()))

// If a period consists of `n` cycles, this function returns a value between 0 and n
let get_current_period_cycle(voting_period_params: voting_period_params) : nat =
  match is_nat(get_current_cycle(voting_period_params) - voting_period_params.cycles_per_period_change_at.1) with
    | Some (elapsed_cycles) -> elapsed_cycles mod voting_period_params.cycles_per_period
    | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
        ("STARTED_ON_IN_FUTURE", ()))
// Checks that we are in proposing period. We split each period of 5 cycles (as of now)
// into two segments. First one for proposing, and second one for voting. The length of
// voting_period, in number of cycles, is configured in store.voting_period.
let is_proposing_segment(store : storage): bool =
  // Compute cycles available for proposing
  let proposing_cycles = match is_nat(store.voting_period_params.cycles_per_period - store.voting_period) with
    | Some (pc) -> pc
    | None -> (failwith("BAD_VOTING_PERIOD") : nat)
  // Check if we are past that many cycles into the period, because then we wont by in proposing period.
  in (get_current_period_cycle(store.voting_period_params) < proposing_cycles)

let get_frozen_tokens_for_current_period(curr_period, addr, store: nat * address * storage) : nat =
  match Big_map.find_opt addr store.freeze_history with
    | Some fh -> if fh.current_period_num = curr_period
        then fh.staked + fh.unstaked
        else 0n
    | None -> 0n
#endif

#endif
