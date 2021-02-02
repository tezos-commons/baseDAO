// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Utility module for contract startup.
//
// To keep origination size small enough we store the entrypoints as lambdas in
// the storage.
// This module exports a 'make_ep_param' function that can create the 'parameter'
// to fill an entrypoint and several 'storable_<ep_name>' functions that are
// lambdas that can be used with said helper function.
// These correspond to their entrypoint code, except for 'balance_of' and
// 'get_vote_permit_counter' (which cannot receive a contract directly so they
// need to get an address instead) as well as 'vote' (that cannot execute the
// 'SELF_ADDRESS' instruction in a lambda, so it needs to receive it as well).


// this is imported only to fill the 'ligo' command's 'ENTRY_POINT' parameter
#include "base_DAO.mligo"

#include "management.mligo"
#include "proposal.mligo"
#include "token.mligo"
#include "token/fa2.mligo"

#include "types.mligo"

// Helpers

let make_ep_param (ep_name, stored_ep : string * storable_entrypoint) : parameter =
  let packed_ep = Bytes.pack stored_ep in
  (M_left (Some (ep_name, Some packed_ep)) : parameter)

// Management

let storable_transfer_ownership (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : transfer_ownership_param option) with
  | Some param -> transfer_ownership(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_accept_ownership (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : unit option) with
  | Some param -> accept_ownership(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_migrate (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : migrate_param option) with
  | Some param -> migrate(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_confirm_migration (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : unit option) with
  | Some param -> confirm_migration(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)


// Proposal

let storable_drop_proposal (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : proposal_key option) with
  | Some param -> drop_proposal(param, config_store.1, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_propose (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : propose_params option) with
  | Some param -> propose(param, config_store.1, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_vote (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : (vote_param_permited list * address) option) with
  | Some param ->
    let (vote_param, ctrt_address) = param in
    vote_with_self(vote_param, ctrt_address, config_store.1, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_set_voting_period (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : voting_period option) with
  | Some param -> set_voting_period(param, config_store.1, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_set_quorum_threshold (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : quorum_threshold option) with
  | Some param -> set_quorum_threshold(param, config_store.1, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_flush (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : nat option) with
  | Some param -> flush(param, config_store.1, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_get_vote_permit_counter (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : (unit * address) option) with
  | Some param ->
      begin
      let (unit_par, callback_address) = param in
      match (Tezos.get_contract_opt(callback_address) : nat contract option) with
      | Some callback_contract ->
        let ep_param = {param = unit_par; callback = callback_contract}
        in get_vote_permit_counter(ep_param, config_store.0)
      | None -> (failwith "UNPACKING_FAILED" : return)
    end
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)


// Token

let storable_burn (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : burn_param option) with
  | Some param -> burn(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_mint (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : mint_param option) with
  | Some param -> mint(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_transfer_contract_tokens (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : transfer_contract_tokens_param option) with
  | Some param -> transfer_contract_tokens(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)


// FA2

let storable_transfer (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : transfer_params option) with
  | Some param -> transfer(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_balance_of (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : (balance_request_item list * address) option) with
  | Some param ->
    begin
      let (request_list, callback_address) = param in
      match (Tezos.get_contract_opt(callback_address) : balance_response_item list contract option) with
      | Some callback_contract ->
        let ep_param = {requests = request_list; callback = callback_contract}
        in balance_of(ep_param, config_store.0)
      | None -> (failwith "UNPACKING_FAILED" : return)
    end
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)

let storable_update_operators (packed_param, config_store : bytes * configured_storage) : return =
  match ((Bytes.unpack packed_param) : update_operators_param option) with
  | Some param -> update_operators(param, config_store.0)
  | None -> (failwith "ENTRYPOINT_NOT_FOUND" : return)
