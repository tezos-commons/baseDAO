// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !COMMON_PLIST_H
#define COMMON_PLIST_H

#include "../types.mligo"

let plist_empty : proposals_doubly_linked_list =
  { first = (None : first_proposal_node option )
  ; last = (None : last_proposal_node option )
  ; map = (Big_map.empty : (proposal_key, (proposal_key * proposal_key)) big_map )
  }

let plist_mem (key, plist : proposal_key * proposals_doubly_linked_list): bool =
  let is_first = match plist.first with
    | None ->
        False
    | Some (first, _) ->
        first = key in
  let is_last = match plist.last with
    | None ->
        False
    | Some (last, _) ->
        prev = key in
  let is_inbetween = Map.mem key plist.map in
  is_first || is_last || is_inbetween

let plist_insert (proposal_key, plist : proposal_key * proposals_doubly_linked_list): proposals_doubly_linked_list =
  match plist.first with
    | None ->
        // If there is no first key
        { plist with
          first = Some (proposal_key, (None: proposal_key option))
        }
    | Some (first, _) ->
        (match plist.last with
          | None ->
              // If there is no last key
              { plist with
                first = Some (first, (Some proposal_key))
              ; last = Some (proposal_key, first)
              }
          | Some (last, prev) ->
              { plist with
                last = Some (proposal_key, last)
              ; map = Map.add last (prev, proposal_key) plist.map
              }
        )

let plist_delete (proposal_key, plist : proposal_key * proposals_doubly_linked_list): proposals_doubly_linked_list =
  let is_first = match plist.first with
    | None ->
        False
    | Some (first, _) ->
        first = key in
  let is_last = match plist.last with
    | None ->
        False
    | Some (last, _) ->
        prev = key in
  

#endif
