// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "defaults.mligo"
#include "types.mligo"
#include "proposal.mligo"
#include "registryDAO/types.mligo"

// The following include is required so that we have a function with an
// entrypoint type to use with `compile-storage` command to make Michelson
// storage expression.
#include "base_DAO.mligo"

type dummy_storage = ((address * address * nat * nat * nat * nat * nat) -> full_storage)

let dummy (action, store : parameter * dummy_storage) : operation list * dummy_storage
  = (([] : operation list), store)

let require_extra_value (key_name, ce : string * contract_extra) : bytes =
    match Map.find_opt key_name ce with
      Some (packed_b) -> packed_b
    | None -> (failwith "CONFIG_VALUE_NOT_FOUND" : bytes)

let require_unpacked_nat_opt (packed_b: bytes) : nat option =
  match ((Bytes.unpack packed_b) : ((nat option) option)) with
    Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : nat option)

let require_unpacked_registry (packed_b: bytes) : registry =
  match ((Bytes.unpack packed_b) : (registry option)) with
    Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : registry)

let require_unpacked_registry_affected (packed_b: bytes) : registry_affected =
  match ((Bytes.unpack packed_b) : (registry_affected option)) with
    Some (v) -> v
  | None -> (failwith "UNPACKING_FAILED" : registry_affected)

let lookup_config (key_name, ce : string * contract_extra) : nat option =
    require_unpacked_nat_opt(require_extra_value(key_name, ce))

let require_config (key_name, ce : string * contract_extra) : nat =
  match lookup_config (key_name, ce) with
    Some (v) -> v
  | None -> (failwith "REQUIRED_CONFIG_NOT_FOUND" : nat)

let apply_diff_registry (diff, registry : registry_diff * registry) : registry =
  let
    foldFn (registry, update: registry * (registry_key * registry_value option)) : registry =
      let registry_key = update.0 in
      let registry_value = update.1 in
      Map.update registry_key registry_value registry
  in List.fold foldFn diff registry

let apply_diff_registry_affected (proposal_key, diff, registry_affected : proposal_key
        * registry_diff * registry_affected) : registry_affected =
  let
    foldFn (registry_affected, update: registry_affected * (registry_key *
      registry_value option)) : registry_affected =
      let registry_key = update.0 in
      Map.update registry_key (Some (proposal_key)) registry_affected
  in List.fold foldFn diff registry_affected

let apply_diff (diff, ce : registry_diff * contract_extra) : contract_extra =
  let registry = require_unpacked_registry(require_extra_value("registry", ce)) in
  let new_registry : registry = apply_diff_registry (diff, registry) in
  Map.update "registry" (Some (Bytes.pack new_registry)) ce

let apply_diff_affected (proposal_key, diff, ce : proposal_key *
    registry_diff * contract_extra) : contract_extra =
  let registry_af = require_unpacked_registry_affected(require_extra_value("registry_affected", ce)) in
  let new_registry_af : registry_affected = apply_diff_registry_affected (proposal_key, diff, registry_af)
  in Map.update "registry_affected" (Some (Bytes.pack new_registry_af)) ce

// Configuration Lambdas

(*
 * Proposal check lambda : returns true if following two checks are true.
 * 1. if proposer has locked a * s + b where s = size(pack(proposalMetadata))
 * and a and b are from storage configuration.
 * 2. proposal size < s_max where s_max is from stored configuration.
 *)
let registry_DAO_proposal_check (params, store : propose_params * storage) : bool =
  let proposal_size = Bytes.size(Bytes.pack(params.proposal_metadata)) in
  let frozen_scale_value = require_config ("a", store.extra) in
  let frozen_extra_value = require_config ("b", store.extra) in
  let max_proposal_size = require_config ("s_max", store.extra) in
  let requred_token_lock = frozen_scale_value * proposal_size + frozen_extra_value
  in (params.frozen_token = requred_token_lock) && (proposal_size < max_proposal_size)

(*
 * Proposal rejection return lambda: returns `c * frozen / d`. where c and d
 * are specified by the DAO creator in configuration and frozen is the amount
 * that was frozen by the proposer.
 *)
let registry_DAO_rejected_proposal_return_value (params, store : proposal * storage) : nat =
  let slash_scale_value = require_config ("c", store.extra) in
  let slash_division_value = require_config ("d", store.extra)
  in (slash_scale_value * params.proposer_frozen_token) / slash_division_value

(*
 * Uses the keys in the proposal metadata to differentiate between configuration
 * proposal and normal proposals. If the map contains `agoraPostID` key, the proposal
 * is considerd as 'normal'. Or else it is considered as carrying a 'configuration'
 * proposal.
 *
 * For 'normal' proposals, the contract_extra is expected to contain the bigmap
 * for registry and bigmap for key update tracking under keys 'registry' and
 * 'registry_affected' keys.
 *
 * For 'configuration' proposal, the code expects all the configuration keys
 * included in the proposal (or it throws an error). The expected keys are "a",
 * "b", "s_max", "c" and "d".  'None' values are ignored and 'Some' values are
 * used for updating corresponding configuraton.
 *)
let registry_DAO_decision_lambda (proposal, store : proposal * storage) : return =
  let propose_param : propose_params = {
    frozen_token = proposal.proposer_frozen_token;
    proposal_metadata = proposal.metadata
    } in
  let proposal_key = to_proposal_key (propose_param, proposal.proposer) in
  match Map.find_opt "agoraPostID" proposal.metadata with
    Some (packed_b) ->
      begin
        match ((Bytes.unpack packed_b) : (nat option)) with
          Some (agora_post_id) ->
            begin
              match Map.find_opt "updates" proposal.metadata with
                Some (packed_b) ->
                  begin
                    match ((Bytes.unpack packed_b) : (registry_diff option)) with
                      Some (diff) ->
                        let ce_after_registry_update = apply_diff(diff, store.extra) in
                        let ce_after_registry_affected_update =
                          apply_diff_affected(proposal_key, diff, ce_after_registry_update) in
                          (([] : operation list), { store with extra = ce_after_registry_affected_update })
                    | None -> (failwith "UPDATES_UNPACKING_FAILED" : return)
                  end
              | None -> (failwith "UPDATES_NOT_FOUND" : return)
            end
        | None -> (failwith "UNPACKING FAILED" : return)
      end
  | None ->
      let m_frozen_scale_value = lookup_config ("a", proposal.metadata) in
      let m_frozen_extra_value = lookup_config ("b", proposal.metadata) in
      let m_max_proposal_size = lookup_config ("s_max", proposal.metadata) in
      let m_slash_scale_value = lookup_config ("c", proposal.metadata) in
      let m_slash_division_value = lookup_config ("d", proposal.metadata) in
      let new_ce = match m_frozen_scale_value with
        Some (frozen_scale_value) -> Map.update "a" (Some (Bytes.pack (frozen_scale_value))) store.extra
        | None -> store.extra in

      let new_ce = match m_frozen_extra_value with
        Some (frozen_scale_value) -> Map.update "b" (Some (Bytes.pack (frozen_scale_value))) new_ce
        | None -> new_ce in

      let new_ce = match m_max_proposal_size with
        Some (max_proposal_size) -> Map.update "s_max" (Some (Bytes.pack (max_proposal_size))) new_ce
        | None -> new_ce in

      let new_ce = match m_slash_scale_value with
        Some (slash_scale_value) -> Map.update "c" (Some (Bytes.pack (slash_scale_value))) new_ce
        | None -> new_ce in

      let new_ce = match m_slash_division_value with
        Some (slash_division_value) -> Map.update "d" (Some (Bytes.pack (slash_division_value))) new_ce
        | None -> new_ce
      in (([] : operation list), { store with extra = new_ce })

let default_registry_DAO_full_storage (admin, token_address, a, b, s_max, c, d
    : (address * address * nat * nat * nat * nat * nat)) : full_storage = let
  fs = default_full_storage (admin, token_address) in
  let new_storage = { fs.0 with
    extra = Map.literal [
          ("registry" , Bytes.pack (Map.empty : registry));
          ("registry_affected" , Bytes.pack (Map.empty : registry_affected));
          ("a" , Bytes.pack a); // frozen_scale_value
          ("b" , Bytes.pack b); // frozen_extra_value
          ("s_max" , Bytes.pack s_max); // max_proposal_size
          ("c" , Bytes.pack c); // slash_scale_value
          ("d" , Bytes.pack d); // slash_division_value
          ];
  } in
  let new_config = { fs.1 with
    proposal_check = registry_DAO_proposal_check;
    rejected_proposal_return_value = registry_DAO_rejected_proposal_return_value;
    decision_lambda = registry_DAO_decision_lambda;
    } in
  (new_storage, new_config)
