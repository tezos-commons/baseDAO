// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !COMMON_ERRORS_H
#define COMMON_ERRORS_H

#include "types.mligo"

let fail_proposal_check (msg : string) : unit =
  ([%Michelson ({| { FAILWITH } |} : (string * string) -> unit)]
    ("FAIL_PROPOSAL_CHECK", msg) : unit)

// Xtz transfer amount cannot be 0
let zero_mutez_err_msg = "ZERO_MUTEZ"

// Xtz transfer amount cannot be smaller than 'min_xtz_amount'
let too_small_xtz_err_msg = "LOW_XTZ"

// Xtz transfer amount cannot be bigger than 'max_xtz_amount'
let too_large_xtz_err_msg = "HIGH_XTZ"

// Incorrect token amounts locked
let wrong_token_amount_err_msg = "WRONG_TOKEN_AMOUNT"

// Proposal size is bigger than 'max_proposal_size'
let large_proposal_err_msg = "LARGE_PROPOSAL"

// Shared between treasury and registry dao
let check_token_locked_and_proposal_size (frozen_token, required_token_lock, proposal_size, max_proposal_size
  : nat * nat * nat * nat) : string option =
    if (frozen_token <> required_token_lock) then Some (wrong_token_amount_err_msg)
    else if (proposal_size >= max_proposal_size) then Some (large_proposal_err_msg)
    else None

let check_xtz_transfer (xt, min_xtz_amount, max_xtz_amount : xtz_transfer * tez * tez) : string option =
  if (xt.amount = 0mutez) then Some zero_mutez_err_msg
  else if (xt.amount < min_xtz_amount) then Some too_small_xtz_err_msg
  else if (xt.amount > max_xtz_amount) then Some too_large_xtz_err_msg
  else None

#endif
