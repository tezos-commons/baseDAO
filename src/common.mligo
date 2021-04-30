// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#if !COMMON_H
#define COMMON_H

#include "types.mligo"

[@inline]
let authorize_admin (store : storage): storage =
  if sender = store.admin then
    store
  else
    (failwith("NOT_ADMIN") : storage)

#if VOTING_POWER_DAO
#include "common/votingpower-dao-common.mligo"
#endif

#endif
