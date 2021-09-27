-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Ligo.BaseDAO.TreasuryDAO.Types
  ( TreasuryCustomEpParam
  ) where

import Ligo.BaseDAO.Types

type instance CustomEpToParam 'Treasury = TreasuryCustomEpParam

type TreasuryCustomEpParam = ()
