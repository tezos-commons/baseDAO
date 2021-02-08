// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Types.hs module

// -- FA2 types -- //

#if !TYPES_H
#define TYPES_H

type token_id = nat

type operator =
  [@layout:comb]
  { owner : address
  ; operator : address
  }
type operators = (operator, unit) big_map

type ledger_key = address * token_id
type ledger_value = nat
type ledger = (ledger_key, ledger_value) big_map

type transfer_destination =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }
type transfer_item =
  [@layout:comb]
  { from_ : address
  ; txs : transfer_destination list
  }
type transfer_params = transfer_item list

type balance_request_item =
  [@layout:comb]
  { owner : address
  ; token_id : token_id
  }
type balance_response_item =
  [@layout:comb]
  { request : balance_request_item
  ; balance : nat
  }
type balance_request_params =
  [@layout:comb]
  { requests : balance_request_item list
  ; callback : balance_response_item list contract
  }

type operator_param =
  [@layout:comb]
  { owner : address
  ; operator : address
  ; token_id : token_id
  }
type update_operator =
  [@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param
type update_operators_param = update_operator list

type fa2_parameter =
  [@layout:comb]
    Transfer of transfer_params
  | Balance_of of balance_request_params
  | Update_operators of update_operators_param

// -- Helpers -- //

type counter =
  { current : nat
  ; total : nat
  }

// -- DAO base types -- //

type nonce = nat

type migration_status =
  [@layout:comb]
  | Not_in_migration
  | MigratingTo of address
  | MigratedTo of address

type proposal_key = bytes
type proposal_metadata = (string, bytes) map
type proposal =
  { upvotes : nat
  ; downvotes : nat
  ; start_date : timestamp
  ; metadata : proposal_metadata
  ; proposer : address
  ; proposer_frozen_token : nat
  ; voters : (address * nat) list
  }

type vote_type = bool

type voting_period = nat
type quorum_threshold = nat

type permit =
  { key : key
  ; signature : signature
  }

type contract_extra = (string, bytes) map

// -- Storage -- //

type storage =
  { ledger : ledger
  ; operators : operators
  ; token_address : address
  ; admin : address
  ; pending_owner : address
  ; metadata : (string, bytes) big_map
  ; migration_status : migration_status
  ; voting_period : voting_period
  ; quorum_threshold : quorum_threshold
  ; extra : contract_extra
  ; proposals : (proposal_key, proposal) big_map
  ; proposal_key_list_sort_by_date : (timestamp * proposal_key) set
  ; permits_counter : nonce
  }

// -- Parameter -- //

type transfer_ownership_param = address

type migrate_param = address

type voting_period = nat
type quorum_threshold = nat

type custom_ep_param = (string * bytes)

type propose_params =
  { frozen_token : nat
  ; proposal_metadata : proposal_metadata
  }

type vote_param =
  [@layout:comb]
  { proposal_key : proposal_key
  ; vote_type : vote_type
  ; vote_amount : nat
  }
type vote_param_permited =
  { argument : vote_param
  ; permit : permit option
  }

type burn_param =
  [@layout:comb]
  { from_ : address
  ; token_id : token_id
  ; amount : nat
  }
type mint_param =
  [@layout:comb]
  { to_ : address
  ; token_id : token_id
  ; amount : nat
  }

type transfer_contract_tokens_param =
  { contract_address : address
  ; params : transfer_params
  }

// TODO: get rid of it once it is possible to execute off-chain-views in tests
type vote_permit_counter_param =
  [@layout:comb]
  { param : unit
  ; callback : nat contract
  }

(*
 * Entrypoints that forbids Tz transfers
 *)
type forbid_xtz_params =
    Call_FA2 of fa2_parameter
  | Drop_proposal of proposal_key
  | Transfer_ownership of transfer_ownership_param
  | Accept_ownership of unit
  | Migrate of migrate_param
  | Confirm_migration of unit
  | Propose of propose_params
  | Vote of vote_param_permited list
  | Set_voting_period of voting_period
  | Set_quorum_threshold of quorum_threshold
  | Flush of nat
  | Burn of burn_param
  | Mint of mint_param
  | GetVotePermitCounter of vote_permit_counter_param

(*
 * Entrypoints that should not work after migration.
 * Separated into entrypoints that forbid Tz transfers and
 * 'CallCustom' which may want to accept Tz transfers.
 *)
type migratable_parameter =
  (custom_ep_param, "CallCustom", forbid_xtz_params, "") michelson_or

(*
 * Full parameter of the running (aka not starting up) contract.
 * Made up of entrypoints that could get migrated, and 'Transfer_contract_tokens'
 * which should keep working even after migration.
 *)
type running_parameter =
  (migratable_parameter, "", transfer_contract_tokens_param, "Transfer_contract_tokens") michelson_or

(*
 * Full parameter of the starting up contract.
 * This is used to load or remove lambdas for the entrypoint into the contract
 * before it can actually be used.
 *
 * The outer-most 'option' is used to either modify an entrypoint ('Some') or
 * put the contract in "running mode" ('None').
 * The 'string' is the name associated to an entrypoint (custom or not)
 * and the 'bytes' should contain the relative packed 'storable_entrypoint' or
 * 'None' to remove it from the contract.
 *)
type startup_parameter = (string * (bytes option)) option

(*
 * Full parameter of the contract.
 * Made of a 'Left' option for a startup process or the 'Right' on to interact
 * with the running contract.
 *)
type parameter = (startup_parameter, "startup", running_parameter, "") michelson_or

// -- Config -- //

type config =
  { proposal_check : propose_params * contract_extra -> bool
  ; rejected_proposal_return_value : proposal * contract_extra -> nat
  ; decision_lambda : proposal * contract_extra -> operation list * contract_extra

  ; max_proposals : nat
  ; max_votes : nat
  ; max_quorum_threshold : nat
  ; min_quorum_threshold : nat
  ; max_voting_period : nat
  ; min_voting_period : nat
  }

type configured_storage = storage * config

(*
 * The type of an entrypoint that can be stored in the storage.
 * Note that it is only able to affect the 'storage' and that it receives its
 * parameter as packed 'bytes'.
 *)
type storable_entrypoint = (bytes * configured_storage) -> (operation list * storage)

(*
 * big_map containing all the entrypoints, indexed by a string
 * representing their name, each being a lambda with access to full_storage and
 * a packed parameter.
 *)
type stored_entrypoints = (string, storable_entrypoint) big_map

(*
 * Part of the 'full_storage' that can only be modified during the startup phase
 * of the contract.
 * It contains the stored 'storable_entrypoint' as well as the startup flag.
 *)
type startup_storage =
  { starting_up : bool
  ; stored_entrypoints : stored_entrypoints
  }

type full_storage = startup_storage * configured_storage

// -- Misc -- //

type return = operation list * storage

let nil_op = ([] : operation list)

#endif  // TYPES_H included
