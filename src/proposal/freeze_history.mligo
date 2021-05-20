// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"


let add_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  { fh with current_unstaked = fh.current_unstaked + amt }

let sub_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  match is_nat(fh.past_unstaked - amt) with
  | Some new_amt -> { fh with past_unstaked = new_amt }
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ( "NOT_ENOUGH_FROZEN_TOKENS", ()) : address_freeze_history)

let stake_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  let fh = sub_frozen_fh(amt, fh) in
  { fh with staked = fh.staked + amt }

let unstake_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  match is_nat(fh.staked - amt) with
  | Some new_amt ->
      // Adding to past_unstaked should be fine since as of now, the staked tokens have to be from
      // past periods.
      { fh with staked = new_amt; past_unstaked = fh.past_unstaked + amt }
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ("NOT_ENOUGH_STAKED_TOKENS", ()) : address_freeze_history)

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
