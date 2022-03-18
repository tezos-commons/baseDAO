// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !COMMON_PLIST_H
#define COMMON_PLIST_H

#include "../types.mligo"

[@inline]
let plist_mem (key, plist_o : proposal_key * proposal_doubly_linked_list option): bool =
  match plist_o with
    | None -> false
    | Some plist ->
        if (key = plist.first)
          then true
          else Map.mem (key, prev) plist.map

[@inline]
let plist_pop (plist_o : proposal_doubly_linked_list option)
    : (proposal_key option * proposal_doubly_linked_list option) =
  match plist_o with
  | None ->
    ( (None : proposal_key option)
    , (None : proposal_doubly_linked_list option)
    )
  | Some plist ->
    let new_plist = match Map.find_opt (plist.first, next) plist.map with
      | Some next_key ->
        Some
          { plist with
            first = next_key // Make the next as the first key
          ; map =
              Map.remove (next_key, prev) // Remove the previous link of the next key.
                (Map.remove (plist.first, next) plist.map) // Remove the first key link.
          }
      | None -> (None : proposal_doubly_linked_list option) // Only the first key exist.
    in (Some plist.first, new_plist)


[@inline]
let plist_insert (key, plist_o : proposal_key * (proposal_doubly_linked_list option)): proposal_doubly_linked_list option =
  match plist_o with
    | None -> Some
        { first = key
        ; last = key
        ; map = (Big_map.empty : ((proposal_key * plist_direction), proposal_key) big_map)
        }
    | Some plist -> Some
          { plist with
            last = key
          ; map =
              Map.add (plist.last, next) key // Add next link of the old last key
                (Map.add (key, prev) plist.last plist.map) // Add previous link of the new last key
          }

[@inline]
let plist_delete (key, plist_o : proposal_key * proposal_doubly_linked_list option): proposal_doubly_linked_list option =
  match plist_o with
    | None ->
        // Do nothing in case of no proposal key.
        plist_o
    | Some plist ->
        // Special case: there is a single key.
        if plist.first = plist.last
        then
          // Either it's a different key or the list gets empty.
          if key = plist.first then None else Some plist
        else
          // We know that there are at least 2 keys in the list
          // find the keys near to key we are looking for (if any):
          let m_prev_key = Map.find_opt (key, prev) plist.map in
          let m_next_key = Map.find_opt (key, next) plist.map in

          // Remove both links from the map already:
          let new_map = Big_map.remove (key, prev)
                (Big_map.remove (key, next) plist.map) in

          // Update the next link of the prev key and find the last key
          let (new_last, new_map) = match m_prev_key with
            | None ->
              // There are no prev key, no changes needed
              (plist.last, new_map)
            | Some prev_key ->
              ( (if plist.last = key then prev_key else plist.last)
              , Big_map.update (prev_key, next) m_next_key new_map
              )
          in
          // Update the prev link of the next key and find the first key
          let (new_first, new_map) = match m_next_key with
            | None ->
              // There are no next key, no changes needed
              (plist.first, new_map)
            | Some next_key ->
              ( (if plist.first = key then next_key else plist.first)
              , Big_map.update (next_key, prev) m_prev_key new_map
              )
          in
          Some { first = new_first; last = new_last; map = new_map }


#endif // COMMON_PLIST_H included
