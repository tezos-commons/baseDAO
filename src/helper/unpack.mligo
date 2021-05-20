// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !HELPER_CONFIG_H
#define HELPER_CONFIG_H

#include "../types.mligo"

(* Required version of `Big_map.find_opt`. Fail immediately if the required key does not exist  *)
let find_big_map (key_name, ce : string * ((string, bytes) big_map)) : (string * bytes) =
  match Big_map.find_opt key_name ce with
  | Some (packed_b) -> (key_name, packed_b)
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> (string * bytes))]
              ("MISSING_VALUE", key_name) : (string * bytes))


(*
 * Unpack Helpers
 *
 * All the `unpack_<type>` functions are required version of `Bytes.unpack`.
 * They each fail immediately if the unpacked result is `None`.
 *)

let unpack_tez(key_name, packed_b: string * bytes) : tez =
  match ((Bytes.unpack packed_b) : (tez option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> tez)]
              ("UNPACKING_FAILED", key_name) : tez)

let unpack_nat(key_name, packed_b: string * bytes) : nat =
  match ((Bytes.unpack packed_b) : (nat option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (string * string) -> nat)]
              ("UNPACKING_FAILED", key_name) : nat)

#endif
