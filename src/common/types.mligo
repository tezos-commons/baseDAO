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

// -- Unpack Helpers (fail if the unpacked result is none) -- //

let unpack_transfer_type_list (key_name, packed_b: string * bytes) : transfer_type list =
  match ((Bytes.unpack packed_b) : (transfer_type list) option) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> transfer_type list)]
              ("UNPACKING_FAILED", key_name) : transfer_type list)

(* Non-failed unpack functions. Accepts an optional bytes, try to unpack if the bytes are not `None`. *)
let unpack_transfer_type_list_safe (bytes_opt: bytes option) : (transfer_type list) option =
  match bytes_opt with
  | None -> (None : (transfer_type list) option)
  | Some b -> (Bytes.unpack b : (transfer_type list) option)

type update_guardian = address

#endif
