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

let unstake_frozen_fh (amt_to_unstake, amt_to_burn, fh : nat * nat * address_freeze_history): address_freeze_history =
  match is_nat(fh.staked - (amt_to_unstake + amt_to_burn)) with
  | Some new_staked_amt ->
      { fh with staked = new_staked_amt; past_unstaked = fh.past_unstaked + amt_to_unstake }
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ("BAD_STATE", ()) : address_freeze_history)

// Update a possibly outdated freeze_history for the current stage
let update_fh (current_stage, freeze_history : nat * address_freeze_history): address_freeze_history =
  if freeze_history.current_stage_num < current_stage
  then
    { current_stage_num = current_stage
    ; staked = freeze_history.staked
    ; current_unstaked = 0n
    ; past_unstaked = freeze_history.current_unstaked + freeze_history.past_unstaked
    }
  else freeze_history
