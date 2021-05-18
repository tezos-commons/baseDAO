-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Integrational.Common
  ( withOriginated
  ) where

import Universum

import Lorentz hiding (take)
import Lorentz.Test

import Data.List.NonEmpty as NE (toList)
import qualified Ligo.BaseDAO.Contract as DAO
import Ligo.BaseDAO.Types
import Michelson.Test.Integrational (tOriginate)

withOriginated
  :: Int
  -> ([Address] -> IntegrationalScenarioM FullStorage)
  -> ([Address] -> TAddress Parameter -> IntegrationalScenarioM a)
  -> IntegrationalScenarioM a
withOriginated addrCount storageFn tests = do
  let addresses = take addrCount $ NE.toList $ genesisAddresses
  storage <- storageFn addresses

  dao <- tOriginate DAO.baseDAOContractLigo "BaseDAOLigo" (toVal storage) (toMutez 0)
  tests addresses (TAddress dao)
