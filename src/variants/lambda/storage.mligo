// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

#include "variants/lambda/types.mligo"

// This type should define the type of the contract-extra field in storage
// for this variant.
type contract_extra = lambda_contract_extra

// Uncomment the following lines and use them to implement proposal handlers
// and proposal check functions, and then use the `lambda_map` defined further
// down to include them in the storage.
let proposal_handler_1 (_ : ph_input) : ph_output = failwith "not-implemented"
let handler_check_1 (_, _ : bytes * handler_storage) : unit = failwith "not-implemented"

let lambda_map : lambdas = Big_map.literal []

// Use the following sample to define each handler instance, and replace the empty list
// above with it.
//   [("handler_1",
//       { code = proposal_handler_1
//       ; handler_check = handler_check_1
//       ; is_active = true
//       }
//    )
//   ]

// This should define a default value for the contract-extra field for the
// variant.
let default_extra : contract_extra =
  { handler_storage = (Map.empty : handler_storage)
  ; lambdas = lambda_map
  }

#endif
