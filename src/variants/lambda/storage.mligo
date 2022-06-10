// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

#include "variants/lambda/types.mligo"

// This type should define the type of the contract-extra field in storage
// for this variant.
type contract_extra = lambda_contract_extra

// This should define a default value for the contract-extra field for the
// variant.
let default_extra : contract_extra =
  { handler_storage = Map.empty
  ; lambdas = Big_map.empty
  }

#endif
