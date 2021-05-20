// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Complete parameter with common signature data and pack them into bytes that
// will be signed in permit.
[@inline]
let vote_param_to_signed_data (param, store : vote_param * storage) : bytes * storage =
  ( Bytes.pack
    ( (Tezos.chain_id, Tezos.self_address)
    , (store.permits_counter, param)
    )
  , { store with permits_counter = store.permits_counter + 1n }
  )

// Verify permit and return its author.
[@inline]
let checked_permit_sender (permit, data_to_sign : permit * bytes): address =
  if Crypto.check permit.key permit.signature data_to_sign
  then Tezos.address (Tezos.implicit_account (Crypto.hash_key (permit.key)))
  else ([%Michelson ({| { FAILWITH } |} : string * bytes -> address)]
        ("MISSIGNED", data_to_sign) : address)

// Check that permit is signed by its author, and return the author
// and the parameter to work with.
[@inline]
let verify_permit_vote
  (permit, vote_param, store : permit * vote_param * storage)
    : (vote_param * address * storage) =
  let (data_to_sign, store) = vote_param_to_signed_data (vote_param, store) in
  let permit_sender = checked_permit_sender (permit, data_to_sign) in
  (vote_param, permit_sender, store)

// Check that permit is signed by its author, and return the data
// carried under permit protection.
[@inline]
let verify_permit_protected_vote
  (permited, store : vote_param_permited * storage)
    : vote_param * address * storage =
  match permited.permit with
  | None -> (permited.argument, Tezos.sender, store)
  | Some permit -> verify_permit_vote (permit, permited.argument, store)
