// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !PARAMETER_H
#define PARAMETER_H
#include "types.mligo"
#include "implementation.mligo"

type allow_xtz_params =
  (allow_xtz_params_contract, "", custom_ep_param, "custom_entrypoints") michelson_or

(*
 * Full parameter of the contract.
 * Separated into entrypoints that forbid Tz transfers,
 * and those that allow Tz transfers
 *)
type parameter =
  (allow_xtz_params, "", forbid_xtz_params, "") michelson_or

#endif
