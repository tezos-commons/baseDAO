// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#include "common.mligo"
#include "types.mligo"
#include "error_codes.mligo"

let make_transfer_on_token (tps, contract_addr : transfer_params * address) : operation =
  let token_contract =
    match (Tezos.get_entrypoint_opt "%transfer" contract_addr : ((transfer_params contract) option)) with
      | Some (c) -> c
      | None -> (failwith bad_token_contract : (transfer_params contract))
  in Tezos.transaction tps 0mutez token_contract

let transfer_contract_tokens (param, store : transfer_contract_tokens_param * storage) : return =
  let store = authorize_admin(store) in
  let operation = make_transfer_on_token(param.params, param.contract_address)
  in (([operation] : operation list), store)
