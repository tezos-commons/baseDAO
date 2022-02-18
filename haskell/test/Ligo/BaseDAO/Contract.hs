-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | LIGO version of the contract.
module Ligo.BaseDAO.Contract
  ( baseDAOContractLigo
  , baseDAORegistryLigo
  , baseDAOTreasuryLigo
  , baseDAORegistryStorageLigo
  , baseDAOTreasuryStorageLigo

  , baseDAOStorageLigo
  , getBaseDAOContract
  ) where

import Universum

import Data.Typeable (eqT, (:~:)(..))

import Test.Cleveland.Lorentz (embedContract)
import Morley.Michelson.Typed
import qualified Lorentz as L

import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.TreasuryDAO.Types
import Ligo.BaseDAO.Types
import Ligo.Util

getBaseDAOContract :: forall cep ce. (Typeable cep, Typeable ce) => Contract (ToT (Parameter' (VariantToParam cep))) (ToT (FullStorage (VariantToExtra ce)))
getBaseDAOContract = case eqT @cep @'Base of
  Just Refl -> baseDAOContractLigo
  Nothing -> case eqT @cep @'Registry of
    Just Refl -> baseDAORegistryLigo
    Nothing -> case eqT @cep @'Treasury of
      Just Refl -> baseDAOTreasuryLigo
      Nothing -> error "Unknown contract"

baseDAOContractLigo :: Contract (ToT Parameter) (ToT BaseFullStorage)
baseDAOContractLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' ()) @FullStorage "resources/testDAO.tz")

baseDAORegistryLigo :: Contract (ToT (Parameter' RegistryCustomEpParam)) (ToT RegistryFullStorage)
baseDAORegistryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' RegistryCustomEpParam) @FullStorage "resources/registryDAO.tz")

baseDAOTreasuryLigo :: Contract (ToT (Parameter' TreasuryCustomEpParam)) (ToT FullStorage)
baseDAOTreasuryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' TreasuryCustomEpParam) @FullStorage "resources/treasuryDAO.tz")

baseDAOStorageLigo :: FullStorage
baseDAOStorageLigo =
  fromVal ($(fetchValue @FullStorage "resources/trivialDAO_storage.tz"))

baseDAORegistryStorageLigo :: FullStorage
baseDAORegistryStorageLigo =
  fromVal ($(fetchValue @FullStorage "resources/registryDAO_storage.tz"))

baseDAOTreasuryStorageLigo :: FullStorage
baseDAOTreasuryStorageLigo =
  fromVal ($(fetchValue @FullStorage "resources/treasuryDAO_storage.tz"))
