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

type total_supply = (token_id, nat) map

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

  // Changing the fixed fee only applies to proposals created after
  // the change, so we need to track the fee that the proposer
  // has actually paid
  ; proposer_fixed_fee_in_token : nat

  ; voters : (address * nat) list
  }

type vote_type = bool

type voting_period = nat
type quorum_threshold = nat

type permit =
  { key : key
  ; signature : signature
  }

type metadata_map = (string, bytes) big_map
type contract_extra = (string, bytes) map

// -- Storage -- //

type storage =
  { ledger : ledger
  ; operators : operators
  ; token_address : address
  ; admin : address
  ; pending_owner : address
  ; metadata : metadata_map
  ; migration_status : migration_status
  ; voting_period : voting_period
  ; quorum_threshold : quorum_threshold
  ; extra : contract_extra
  ; proposals : (proposal_key, proposal) big_map
  ; proposal_key_list_sort_by_date : (timestamp * proposal_key) set
  ; permits_counter : nonce
  ; total_supply : total_supply
  ; fixed_proposal_fee_in_token : nat
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

type get_total_supply_param =
  [@layout:comb]
  { token_id : token_id
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
  | Vote of vote_param_permited list
  | Set_fixed_fee_in_token of nat
  | Set_voting_period of voting_period
  | Set_quorum_threshold of quorum_threshold
  | Flush of nat
  | Burn of burn_param
  | Mint of mint_param
  | GetVotePermitCounter of vote_permit_counter_param
  | Get_total_supply of get_total_supply_param

(*
 * Entrypoints that allow Tz transfers
 *)
type allow_xtz_params =
  | CallCustom of custom_ep_param
  | Propose of propose_params

(*
 * Entrypoints that should not work after migration.
 * Separated into entrypoints that forbid Tz transfers,
 * and those that allow Tz transfers
 *)
type migratable_parameter =
  (allow_xtz_params, "", forbid_xtz_params, "") michelson_or

(*
 * Full parameter of the contract.
 * Made up of entrypoints that could get migrated, and 'Transfer_contract_tokens'
 * which should keep working even after migration.
 *)
type parameter =
  (migratable_parameter, "", transfer_contract_tokens_param, "Transfer_contract_tokens") michelson_or

type custom_entrypoints = (string, bytes) map

type decision_lambda_input =
  { proposal : proposal
  ; storage : storage
  }

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

  ; custom_entrypoints : custom_entrypoints
  }

type full_storage = storage * config

// -- Misc -- //

type return = operation list * storage

type return_with_full_storage = operation list * full_storage

let nil_op = ([] : operation list)

let unfrozen_token_id: nat = 0n

let frozen_token_id: nat = 1n

#endif  // TYPES_H included
