// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Token.hs module

#include "common.mligo"
#include "types.mligo"

let make_transfer_on_token (tps, contract_addr : transfer_params * address) : operation =
  let token_contract =
    match (Tezos.get_entrypoint_opt "%transfer" contract_addr : ((transfer_params contract) option)) with
      | Some (c) -> c
      | None -> (failwith "BAD_TOKEN_CONTRACT" : (transfer_params contract))
  in Tezos.transaction tps 0mutez token_contract

let transfer_contract_tokens (param, store : transfer_contract_tokens_param * storage) : return =
  let store = authorize_admin(store) in
  let operation = make_transfer_on_token(param.params, param.contract_address)
  in (([operation] : operation list), store)
