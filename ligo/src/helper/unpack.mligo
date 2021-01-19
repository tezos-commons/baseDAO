// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !HELPER_CONFIG_H
#define HELPER_CONFIG_H

#include "../types.mligo"

// ------------------------------
// Unpack Helpers
// ------------------------------

let unpack_key (key_name, p : string * ((string, bytes) map)) : bytes =
    match Map.find_opt key_name p with
      Some (b) -> b
    | None -> (failwith "UNPACK_KEY_NOT_FOUND" : bytes)

let unpack_nat (key_name, p : string * ((string, bytes) map)) : nat =
  let b = unpack_key (key_name, p)
  in match ((Bytes.unpack b) : (nat option)) with
    Some (v) -> v
  | None -> (failwith "UNPACKING_NOT_NAT_TYPE" : nat)

let unpack_tez (key_name, p : string * ((string, bytes) map)) : tez =
  let b = unpack_key (key_name, p)
  in match ((Bytes.unpack b) : tez option) with
    Some (v) -> v
  | None -> (failwith "UNPACKING_NOT_TEZ_TYPE" : tez)

#endif
