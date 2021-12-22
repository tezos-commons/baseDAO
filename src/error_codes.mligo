// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// NOTE: This file should not be modified directly.
// Use `stack scripts/generate_error_code.hs` instead.

#if ERRORS_CODE_MLIGO
#else
#define ERRORS_CODE_MLIGO

#include "types.mligo"

 // ---------------------------------------------------------------------------
// -- Invalid Input Error Codes
// ---------------------------------------------------------------------------

(* The sender is not the administrator. *)
[@inline] let not_admin = 100n

(* The sender is not the current pending administrator. *)
[@inline] let not_pending_admin = 101n

(* Thrown paired with a `string` error message when the proposal does not pass the `proposal_check`. *)
[@inline] let fail_proposal_check = 102n

(* The proposal does not exist or is no longer ongoing. *)
[@inline] let proposal_not_exist = 103n

(* The proposal voting stage has already ended. *)
[@inline] let voting_stage_over = 104n

(* Transfer of XTZ is forbidden on this entrypoint. *)
[@inline] let forbidden_xtz = 107n

(* The submitted proposal already exist. *)
[@inline] let proposal_not_unique = 108n

(* Parameter signature does not match the expected one - for permits. *)
[@inline] let missigned = 109n

(* The unpacking of a submitted value failed. *)
[@inline] let unpacking_failed = 110n

(* The unpacking of a proposal metadata failed. *)
[@inline] let unpacking_proposal_metadata_failed = 111n

(* A required field value was not found. *)
[@inline] let missing_value = 112n

(* Proposals cannot be submitted in non-proposing stages. *)
[@inline] let not_proposing_stage = 113n

(* There aren't enough frozen tokens for the operation. *)
[@inline] let not_enough_frozen_tokens = 114n

(* The governance token contract does not have the expected entrypoints. *)
[@inline] let bad_token_contract = 115n

(* The type of a contract for a view entrypoint is not of the expected type. *)
[@inline] let bad_view_contract = 116n

(* The conditions to drop this proposal are not met. *)
[@inline] let drop_proposal_condition_not_met = 117n

(* The proposal has expired and can no longer be flushed. *)
[@inline] let expired_proposal = 118n

(* There are no available proposals to flush. *)
[@inline] let empty_flush = 119n

(* The sender has not been delegated the control of the required tokens. *)
[@inline] let not_delegate = 120n

(* Executing the proposal's decision lambda results in failure. *)
[@inline] let fail_decision_lambda = 121n

(* Cannot call `unstake_vote` on the proposal that is not flushed or dropped. *)
[@inline] let unstake_invalid_proposal = 122n

(* The sender did not vote on the proposal or already unstaked tokens from the proposal. *)
[@inline] let voter_does_not_exist = 123n






 // ---------------------------------------------------------------------------
// -- Internal Error Codes
// ---------------------------------------------------------------------------

(* Throw when storage is in an unexpected state, indicating a contract error. *)
[@inline] let bad_state = 300n




#endif
