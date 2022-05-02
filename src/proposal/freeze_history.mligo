// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "../types.mligo"
#include "../error_codes.mligo"


let add_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  { fh with frozen_tokens = fh.frozen_tokens + amt }

let sub_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  match is_nat(fh.frozen_tokens - amt) with
  | Some new_amt -> { fh with frozen_tokens = new_amt }
  | None ->
      (failwith not_enough_frozen_tokens : address_freeze_history)

let stake_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  sub_frozen_fh(amt, fh)

let unstake_frozen_fh (amt_to_unstake, fh : nat * address_freeze_history): address_freeze_history =
  { fh with frozen_tokens = fh.frozen_tokens + amt_to_unstake ; }
