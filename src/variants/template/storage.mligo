// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

// This type should define the type of the contract-extra field in storage
// for this variant.
type contract_extra = unit

// This should define a default value for the contract-extra field for the
// variant.
let default_extra : contract_extra = unit

#endif
