// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !COMMON_TYPES_H
#define COMMON_TYPES_H

#include "../types.mligo"

// -- Transfer -- //

type xtz_transfer =
  [@layout:comb]
  { amount : tez
  ; recipient : address
  }

type token_transfer =
  [@layout:comb]
  { contract_address : address
  ; transfer_list : transfer_item list
  }

type transfer_type =
  [@layout:comb]
  | Xtz_transfer_type of xtz_transfer
  | Token_transfer_type of token_transfer

#endif
