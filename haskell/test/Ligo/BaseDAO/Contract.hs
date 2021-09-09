-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | LIGO version of the contract.
module Ligo.BaseDAO.Contract
  ( baseDAOContractLigo

  , baseDAORegistryStorageLigo
  , baseDAOTreasuryStorageLigo
  ) where

import Michelson.Test.Import (embedContract)
import Michelson.Typed

import Ligo.BaseDAO.Types
import Ligo.Util

baseDAOContractLigo :: Contract (ToT Parameter) (ToT FullStorage)
baseDAOContractLigo =
  $$(embedContract @(ToT Parameter) @(ToT FullStorage) "resources/baseDAO.tz")

baseDAORegistryStorageLigo :: FullStorage
baseDAORegistryStorageLigo =
  fromVal ($(fetchValue @FullStorage "resources/registryDAO_storage.tz"))

baseDAOTreasuryStorageLigo :: FullStorage
baseDAOTreasuryStorageLigo =
  fromVal ($(fetchValue @FullStorage "resources/treasuryDAO_storage.tz"))
