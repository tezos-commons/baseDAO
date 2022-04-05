// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT
#define VARIANT

#include "types.mligo"

let proposal_check (_, _ : propose_params * contract_extra) : unit = unit
let decision_callback (input : decision_callback_input)
    : decision_callback_output = { operations = ([] : operation list); extras = input.extras; guardian = (None : address option) }
let rejected_proposal_slash_value (_, _ : proposal * contract_extra) : nat = 1n

type custom_ep_param = unit
let custom_ep (_, storage : custom_ep_param * storage): operation list * storage
  = (([] : operation list), storage)
#endif
