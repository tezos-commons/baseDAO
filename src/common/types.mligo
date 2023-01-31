// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !COMMON_TYPES_H
#define COMMON_TYPES_H

#include "../types.mligo"
#include "../error_codes.mligo"

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

type legacy_transfer_target =
  [@layout:comb]
  { to : address
  ; value : nat
  }

type legacy_transfer =
  [@layout:comb]
  { from : address
  ; target : legacy_transfer_target
  }

type legacy_token_transfer =
  [@layout:comb]
  { contract_address : address
  ; transfer : legacy_transfer
  }

type transfer_type =
  [@layout:comb]
  | Xtz_transfer_type of xtz_transfer
  | Token_transfer_type of token_transfer
  | Legacy_token_transfer_type of legacy_token_transfer

// Unpack Helper (fail if the unpacked result is none)
let unpack_transfer_type_list (key_name, packed_b: string * bytes) : transfer_type list =
  match ((Bytes.unpack packed_b) : (transfer_type list) option) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (nat * string) -> transfer_type list)]
              (unpacking_failed, key_name) : transfer_type list)

(*
 * Non-failing unpack function.
 * Accepts an optional bytes, tries to unpack if the bytes are not `None`.
 *)
let unpack_transfer_type_list_safe (bytes_opt: bytes option) : (transfer_type list) option =
  match bytes_opt with
  | None -> (None : (transfer_type list) option)
  | Some b -> (Bytes.unpack b : (transfer_type list) option)

// -- Guardian -- //

type update_guardian = address

// -- Delegate -- //
type update_contract_delegate = key_hash option

#endif
