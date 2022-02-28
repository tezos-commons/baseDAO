// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "../../../../src/common/types.mligo"
#include "../../../../src/proposal/plist.mligo"

type plist_parameter =
  | Insert of proposal_key
  | Delete of proposal_key
  | Mem of proposal_key
  | Pop of unit

type plist_storage =
  { plist : (proposal_doubly_linked_list option)
  ; mem_result: bool
  ; pop_result: proposal_key option
  }

let plist_contract (param, store : plist_parameter * plist_storage)
    : operation list * plist_storage =
  let new_store = match param with
        | Insert k -> { store with plist = plist_insert (k, store.plist) }
        | Delete k -> { store with plist = plist_delete (k, store.plist) }
        | Mem k    -> { store with
            mem_result = plist_mem (k, store.plist)
          }
        | Pop _    ->
            let (pop_result, plist) = plist_pop store.plist
            in { store with plist = plist ; pop_result = pop_result}
  in (nil_op, new_store)
