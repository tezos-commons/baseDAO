// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !HELPER_CONFIG_H
#define HELPER_CONFIG_H

#include "../types.mligo"
#include "../error_codes.mligo"

(*
 * Required version of `Big_map.find_opt`.
 * Fails immediately if the required key does not exist.
 *)
let find_big_map (key_name, ce : string * ((string, bytes) big_map)) : (string * bytes) =
  match Big_map.find_opt key_name ce with
  | Some (packed_b) -> (key_name, packed_b)
  | None -> ([%Michelson ({| { FAILWITH } |} : (nat * string) -> (string * bytes))]
              (missing_value, key_name) : (string * bytes))


(*
 * Unpack Helpers
 *
 * They each fail immediately if the unpacked result is `None`.
 *)

let unpack_tez(key_name, packed_b: string * bytes) : tez =
  match ((Bytes.unpack packed_b) : (tez option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (nat * string) -> tez)]
              (unpacking_failed, key_name) : tez)

let unpack_nat(key_name, packed_b: string * bytes) : nat =
  match ((Bytes.unpack packed_b) : (nat option)) with
  | Some (v) -> v
  | None -> ([%Michelson ({| { FAILWITH } |} : (nat * string) -> nat)]
              (unpacking_failed, key_name) : nat)

#endif
