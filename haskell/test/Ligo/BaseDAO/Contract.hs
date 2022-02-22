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

getBaseDAOContract :: forall cep. (Typeable cep) => Contract (ToT (Parameter' (VariantToParam cep))) (ToT (FullStorageSkeleton (VariantToExtra cep)))
getBaseDAOContract = case eqT @cep @'Base of
  Just Refl -> baseDAOContractLigo
  Nothing -> case eqT @cep @'Registry of
    Just Refl -> baseDAORegistryLigo
    Nothing -> case eqT @cep @'Treasury of
      Just Refl -> baseDAOTreasuryLigo
      Nothing -> error "Unknown contract"

baseDAOContractLigo :: Contract (ToT Parameter) (ToT FullStorage)
baseDAOContractLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' ()) @FullStorage "resources/trivialDAO.tz")

baseDAORegistryLigo :: Contract (ToT (Parameter' RegistryCustomEpParam)) (ToT RegistryFullStorage)
baseDAORegistryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' RegistryCustomEpParam) @RegistryFullStorage "resources/registryDAO.tz")

baseDAOTreasuryLigo :: Contract (ToT (Parameter' TreasuryCustomEpParam)) (ToT TreasuryFullStorage)
baseDAOTreasuryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' TreasuryCustomEpParam) @TreasuryFullStorage "resources/treasuryDAO.tz")

baseDAOStorageLigo :: FullStorage
baseDAOStorageLigo =
  fromVal ($(fetchValue @FullStorage "resources/trivialDAO_storage.tz"))

baseDAORegistryStorageLigo :: RegistryFullStorage
baseDAORegistryStorageLigo =
  fromVal ($(fetchValue @RegistryFullStorage "resources/registryDAO_storage.tz"))

baseDAOTreasuryStorageLigo :: TreasuryFullStorage
baseDAOTreasuryStorageLigo =
  fromVal ($(fetchValue @TreasuryFullStorage "resources/treasuryDAO_storage.tz"))
