-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Integrational.Common
  ( withOriginated
  ) where

import Universum

import Lorentz
import Lorentz.Test

import qualified Ligo.BaseDAO.Contract as DAO
import Ligo.BaseDAO.Types
import Michelson.Test.Integrational (tOriginate)
import Data.List.NonEmpty as NE (toList)

withOriginated
  :: Int
  -> ([Address] -> FullStorage)
  -> ([Address] -> TAddress ParameterL -> IntegrationalScenarioM a)
  -> IntegrationalScenarioM a
withOriginated addrCount storageFn tests = do
  let addresses = take addrCount $ NE.toList $ genesisAddresses
  let storage = storageFn addresses
  dao <- tOriginate DAO.baseDAOContractLigo "BaseDAOLigo" (toVal storage) (toMutez 0)
  tests addresses (TAddress dao)
