// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

#if !COMMON_H
#define COMMON_H

#include "error_codes.mligo"
#include "types.mligo"

[@inline]
let authorize_admin (store : storage): storage =
  if Tezos.get_sender unit = store.admin then
    store
  else
    (failwith not_admin : storage)

#endif
