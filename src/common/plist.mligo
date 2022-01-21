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
        false
    | Some (first, _) ->
        first = key in
  let is_last = match plist.last with
    | None ->
        false
    | Some (last, _) ->
        last = key in
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
  match plist.first with
    | None ->
        // Expect to have at least a proposal to delete it.
        (failwith bad_state : proposals_doubly_linked_list)
    | Some (first, _) ->
        (match plist.last with
          | None ->
              if (first = proposal_key) then
                // There should be only the first
                { plist with
                  first = (None : first_proposal_node option)
                }
              else
                // Cannot delete a key that does not exist. (TODO: Fail silently or throw error?)
                (failwith bad_state : proposals_doubly_linked_list)
          | Some (last, prev) ->
              if (last = proposal_key) then
                let (prev_prev, _) =
                      match Map.find_opt prev plist.map with
                        | Some p -> p
                        | None -> (failwith bad_state : (proposal_key * proposal_key)) in
                { plist with
                  last = Some (prev, prev_prev) // Replace the "next to last" with the "last".
                ; map = Map.remove last plist.map // Remove the "last" .
                }
              else
                // Check if the key exist first
                let (prev, next) =
                    match Map.find_opt proposal_key plist.map with
                      | Some p -> p
                      | None ->
                          // Key does not exist (TODO: Fail silently or throw error?)
                          (failwith bad_state : (proposal_key * proposal_key)) in
                let prev_data = match Map.find_opt prev plist.map with
                      | Some p -> p
                      | None -> (failwith bad_state : (proposal_key * proposal_key)) in // Shouldn't happen
                let next_data = match Map.find_opt next plist.map with
                      | Some p -> p
                      | None -> (failwith bad_state : (proposal_key * proposal_key)) in // Shouldn't happen
                // Remove the key
                let new_map = Map.remove proposal_key plist.map in

                // Update the previous index
                let new_map = Map.add prev (prev_data.0, next) new_map in

                // Update the next index
                let new_map = Map.add next (prev, next_data.1) new_map in

                { plist with
                  map = new_map
                }
        )

// flush helper function that modify the `proposals_doubly_linked_list`.
let plist_flush (store : storage): storage =
  // operate on the first

  // operate on the next

  // operate on the last (if Map.find_opt `next` does not exist in the `map`, check it in the `last`. If it exist, then flush it.) (flush = run unstake f, and update the head)

  (failwith "" : storage)

#endif
