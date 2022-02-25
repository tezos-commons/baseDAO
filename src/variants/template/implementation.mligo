// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !VARIANT
#define VARIANT

#include "types.mligo"

// -----------------------------------------------------------------
// Proposal check
//
// This should implement the proposal_check logic of the variant and should
// verify whether a proposal can be submitted.  It checks 2 things: the
// proposal itself and the amount of tokens frozen upon submission.  It allows
// the DAO to reject a proposal by arbitrary logic and captures bond
// requirements
// -----------------------------------------------------------------

let proposal_check (_, _ : propose_params * contract_extra) : unit = unit

// -----------------------------------------------------------------
// decision_callback
//
// The decision callback is executed based on a successful proposal.  It has
// access to the proposal, can modify `contractExtra` and perform arbitrary
// operations.
// -----------------------------------------------------------------

let decision_callback (input : decision_callback_input)
    : decision_callback_output = { operations = ([] : operation list); extras = input.extras; guardian = (None : address option) }

// -----------------------------------------------------------------
// Rejected proposal slash value is called when a proposal is rejected, and the
// value that voters get back can be slashed.  This procedure should return the
// amount to be slashed.
// -----------------------------------------------------------------

let rejected_proposal_slash_value (_, _ : proposal * contract_extra) : nat = 0n

// -----------------------------------------------------------------
// Type that make up the custom entrypoint of the variant.
//
// -----------------------------------------------------------------
type custom_ep_param = unit

// -----------------------------------------------------------------
// Custom entrypoint handler for this variant
//
// -----------------------------------------------------------------
let custom_ep (_, storage, _ : custom_ep_param * storage * config): operation list * storage
  = (([] : operation list), storage)

#endif
