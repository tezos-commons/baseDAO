// SPDX-FileCopyrightText: 2022 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !VARIANT_STORAGE
#define VARIANT_STORAGE

#include "variants/lambda/types.mligo"
#include "error_codes.mligo"

// Xtz transfer amount cannot be 0
let zero_mutez_err_msg = "ZERO_MUTEZ"

// Xtz transfer amount cannot be smaller than `min_xtz_amount`
let too_small_xtz_err_msg = "LOW_XTZ"

// Xtz transfer amount cannot be bigger than `max_xtz_amount`
let too_large_xtz_err_msg = "HIGH_XTZ"

let fail_proposal_check_ (msg : string) : unit =
  ([%Michelson ({| { FAILWITH } |} : (nat * string) -> unit)]
    (fail_proposal_check, msg) : unit)

type update_receiver_param =
  | Add_receivers of (address list)
  | Remove_receivers of (address list)

type config_proposal =
  { frozen_scale_value : nat option
  ; frozen_extra_value : nat option
  ; slash_scale_value : nat option
  ; slash_division_value : nat option
  ; max_proposal_size : nat option
  }

type xtz_transfer =
  [@layout:comb]
  { amount : tez
  ; recipient : address
  }

type token_id = nat
// FA2 transfer types
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

type token_transfer =
  [@layout:comb]
  { contract_address : address
  ; transfer_list : transfer_item list
  }

type legacy_transfer_target =
  [@layout:comb]
  { to : address
  ; value : nat
  }

type legacy_transfer =
  [@layout:comb]
  { from : address
  ; target : legacy_transfer_target
  }

type legacy_token_transfer =
  [@layout:comb]
  { contract_address : address
  ; transfer : legacy_transfer
  }

type transfer_type =
  [@layout:comb]
  | Xtz_transfer_type of xtz_transfer
  | Token_transfer_type of token_transfer
  | Legacy_token_transfer_type of legacy_token_transfer

type proposal_receivers = address set
type proposal_key = bytes
type registry_key = string
type registry_value = string
type registry_diff = (registry_key * registry_value option) list
type registry = (registry_key, registry_value) map
type registry_affected = (registry_key, proposal_key) map
type transfer_proposal =
  { agora_post_id : nat
  ; transfers : transfer_type list
  ; registry_diff : registry_diff
  }

// This type should define the type of the contract-extra field in storage
// for this variant.
type contract_extra =
  { handler_storage : handler_storage
  ; lambdas : lambdas
  }

type transfer_params = transfer_item list

let handle_transfer (ops, transfer_type : (operation list) * transfer_type) : (operation list) =
  match transfer_type with
  | Legacy_token_transfer_type tt ->
    begin
      match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
          : (legacy_transfer contract) option) with
      | Some contract ->
          (Tezos.transaction tt.transfer 0mutez contract) :: ops
      | None -> (failwith fail_decision_callback : operation list)
    end
  | Token_transfer_type tt ->
    begin
      match (Tezos.get_entrypoint_opt "%transfer" tt.contract_address
          : transfer_params contract option) with
      | Some contract ->
          (Tezos.transaction tt.transfer_list 0mutez contract) :: ops
      | None -> (failwith fail_decision_callback : operation list)
    end
  | Xtz_transfer_type xt ->
    begin
      match (Tezos.get_contract_opt xt.recipient : unit contract option) with
      | Some contract ->
          (Tezos.transaction unit xt.amount contract) :: ops
      | None -> (failwith fail_decision_callback : operation list)
    end


let to_proposal_key (propose_params: propose_params): proposal_key =
  Crypto.blake2b (Bytes.pack propose_params)

let fetch_proposal_receivers (hs : handler_storage) : proposal_receivers =
  match (Map.find_opt "proposal_receivers" hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : proposal_receivers option) with
        | Some v -> v
        | _ -> failwith "proposal_receivers decoding failed")
    | _ -> failwith "proposal_receivers not found"

let fetch_registry (hs : handler_storage) : registry =
  match (Map.find_opt "registry" hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : registry option) with
        | Some reg -> reg
        | _ -> failwith "registry decoding failed")
    | _ -> failwith "registry not found"

let fetch_registry_affected (hs : handler_storage) : registry_affected =
  match (Map.find_opt "registry_affected" hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : registry_affected option) with
        | Some reg -> reg
        | _ -> failwith "registry_affected decoding failed")
    | _ -> failwith "registry_affected data not found"

let apply_diff_registry (diff, registry : registry_diff * registry) : registry =
  let
    foldFn (registry, update: registry * (registry_key * registry_value option)) : registry =
      let (registry_key, registry_value) = update in
      Map.update registry_key registry_value registry
  in List.fold foldFn diff registry

let apply_diff_registry_affected
  (proposal_key, diff, registry_affected : proposal_key * registry_diff * registry_affected)
    : registry_affected =
  let
    foldFn (registry_affected, update: registry_affected * (registry_key *
      registry_value option)) : registry_affected =
      let registry_key = update.0 in
      Map.update registry_key (Some (proposal_key)) registry_affected
  in List.fold foldFn diff registry_affected

let apply_diff (rdiff, hs : registry_diff * handler_storage) : handler_storage =
  let registry = fetch_registry(hs) in
  let new_registry : registry = apply_diff_registry (rdiff, registry) in
  Map.update "registry" (Some (Bytes.pack new_registry)) hs

let apply_diff_affected (pk, rdiff, hs : proposal_key * registry_diff * handler_storage) : handler_storage =
  let registry_af = fetch_registry_affected(hs) in
  let new_registry_af : registry_affected = apply_diff_registry_affected (pk, rdiff, registry_af) in
  Map.update "registry_affected" (Some (Bytes.pack new_registry_af)) hs

// Uncomment the following lines and use them to implement proposal handlers
// and proposal check functions, and then use the `lambda_map` defined further
// down to include them in the storage.
let transfer_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : transfer_proposal option) with
    | Some tp ->
      let registry_diff = tp.registry_diff in
      let proposal_key = to_proposal_key(ph_input.proposal_info) in
      let hs = apply_diff(registry_diff, ph_input.handler_storage) in
      let hs = apply_diff_affected(proposal_key, registry_diff, hs) in

      // handle transfers
      let transfers = tp.transfers in
      let ops = List.fold handle_transfer transfers ([] : operation list) in
      { operations = ops; handler_storage = hs; guardian = None }
    | None -> failwith "decoding transfer_proposal failed"

let fetch_tez(key, hs : string * handler_storage) : tez =
  match (Map.find_opt key hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : tez option) with
        | Some v -> v
        | _ -> failwith "decoding Tez value failed")
    | _ -> failwith "expected Tez value was not found"

let fetch_nat(key, hs : string * handler_storage) : nat =
  match (Map.find_opt key hs : bytes option) with
    | Some pr -> (match (Bytes.unpack pr : nat option) with
        | Some v -> v
        | _ -> failwith "decoding of Nat value failed")
    | _ -> failwith "expected nat value was not found"

let check_xtz_transfer (xt, min_xtz_amount, max_xtz_amount : xtz_transfer * tez * tez) : string option =
  if (xt.amount = 0mutez) then Some zero_mutez_err_msg
  else if (xt.amount < min_xtz_amount) then Some too_small_xtz_err_msg
  else if (xt.amount > max_xtz_amount) then Some too_large_xtz_err_msg
  else None

let transfer_proposal_handler_check (ptp, hs : bytes * handler_storage) : unit =
  match (Bytes.unpack ptp : transfer_proposal option) with
    | Some tp ->
      let min_xtz_amount = fetch_tez("min_xtz_amount", hs) in
      let max_xtz_amount = fetch_tez("max_xtz_amount", hs) in
      let is_all_transfers_valid (transfer_type: transfer_type) =
        match transfer_type with
        | Legacy_token_transfer_type _tt -> unit
        | Token_transfer_type _tt -> unit
        | Xtz_transfer_type xt ->
            begin
              match check_xtz_transfer(xt, min_xtz_amount, max_xtz_amount) with
              | Some err_msg -> fail_proposal_check_(err_msg)
              | None -> unit
            end
      in
        List.iter is_all_transfers_valid tp.transfers
    | None -> failwith "transfer_proposal decoding failed"

let set_hs_nat(key, val, hs : string * nat * handler_storage) : handler_storage =
  Map.update key (Some (Bytes.pack val)) hs

let set_recievers(val, hs : (address set) * handler_storage) : handler_storage =
  Map.update "proposal_receivers"  (Some (Bytes.pack val)) hs

let configuration_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : config_proposal option) with
    | Some cp ->
        let new_hs = match cp.frozen_scale_value with
          | Some (frozen_scale_value) ->
              set_hs_nat ("frozen_scale_value", frozen_scale_value, ph_input.handler_storage)
          | None -> ph_input.handler_storage in

        let new_hs = match cp.frozen_extra_value with
          | Some (frozen_extra_value) ->
              set_hs_nat ("frozen_extra_value", frozen_extra_value, new_hs)
          | None -> new_hs in

        let new_hs = match cp.max_proposal_size with
          | Some (max_proposal_size) ->
              set_hs_nat ("max_proposal_size", max_proposal_size, new_hs)
          | None -> new_hs in

        let new_hs = match cp.slash_scale_value with
          | Some (slash_scale_value) ->
              set_hs_nat ("slash_scale_value", slash_scale_value, new_hs)
          | None -> new_hs in

        let new_hs = match cp.slash_division_value with
          | Some (slash_division_value) ->
              set_hs_nat ("slash_division_value", slash_division_value, new_hs)
          | None -> new_hs
        in { operations = []; handler_storage = new_hs; guardian = None }
    | None -> failwith "configuration decoding failed"

let configuration_proposal_handler_check (_, _ : bytes * handler_storage) : unit = unit

let update_guardian_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : address option) with
    | Some guardian -> { operations = []; handler_storage = ph_input.handler_storage; guardian = Some guardian }
    | None -> failwith "decoding guardian address failed"

let update_guardian_handler_check (_, _ : bytes * handler_storage) : unit = unit

let update_contract_delegate_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : (key_hash option) option) with
    | Some mdelegate -> { operations = [Tezos.set_delegate mdelegate]; handler_storage = ph_input.handler_storage; guardian = None }
    | None -> failwith "decoding contract delegate failed"

let update_contract_delegate_handler_check (_, _ : bytes * handler_storage) : unit = unit

type update_fn = (address set * address) -> proposal_receivers

let update_receivers(current_set, updates, update_fn : proposal_receivers * address list * update_fn)
    : proposal_receivers =
  List.fold update_fn updates current_set

let update_receivers_check (_, _ : bytes * handler_storage) : unit = unit

let update_receivers_proposal_handler (ph_input : ph_input) : ph_output =
  match (Bytes.unpack ph_input.packed_argument : update_receiver_param option) with
    | Some urp ->
        let current_set = fetch_proposal_receivers(ph_input.handler_storage) in
        let new_set = match urp with
          | Add_receivers receivers ->
              update_receivers(current_set, receivers, (fun (c, i : address set * address) -> Set.add i c))
          | Remove_receivers receivers ->
              update_receivers(current_set, receivers, (fun (c, i : address set * address) -> Set.remove i c))
        in { operations = []
           ; handler_storage = set_recievers(new_set, ph_input.handler_storage)
           ; guardian = (None : (address option))
           }
    | None -> failwith "decoding update_receiver_param failed"

let lambda_map : lambdas = Big_map.literal
  [ ("transfer_proposal", { code = transfer_proposal_handler; handler_check = transfer_proposal_handler_check; is_active = true })
  ; ("configuration_proposal", { code = configuration_proposal_handler; handler_check = configuration_proposal_handler_check; is_active = true })
  ; ("update_guardian_proposal", { code = update_guardian_proposal_handler; handler_check = update_guardian_handler_check; is_active = true })
  ; ("update_contract_delegate_proposal", { code = update_contract_delegate_proposal_handler; handler_check = update_contract_delegate_handler_check; is_active = true })
  ; ("update_receivers_proposal", { code = update_receivers_proposal_handler; handler_check = update_receivers_check; is_active = true })
  ]

// Use the following sample to define each handler instance, and replace the empty list
// above with it.
//   [("handler_1",
//       { code = proposal_handler_1
//       ; handler_check = handler_check_1
//       ; is_active = true
//       }
//    )
//   ]

// This should define a default value for the contract-extra field for the
// variant.
let default_extra : contract_extra =
  { handler_storage = (Map.empty : handler_storage)
  ; lambdas = lambda_map
  }

#endif
