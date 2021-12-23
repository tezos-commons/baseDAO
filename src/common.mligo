// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !COMMON_H
#define COMMON_H

#include "error_codes.mligo"
#include "types.mligo"

[@inline]
let authorize_admin (store : storage): storage =
  if sender = store.admin then
    store
  else
    (failwith not_admin : storage)

#endif
