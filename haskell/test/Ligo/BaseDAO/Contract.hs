-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

-- | LIGO version of the contract.
module Ligo.BaseDAO.Contract
  ( baseDAOContractLigo
  , baseDAORegistryLigo
  , baseDAOTreasuryLigo
  , baseDAOLambdaRegistryLigo
  , baseDAOLambdaTreasuryLigo
  , baseDAOLambdaLigo
  , baseDAORegistryStorageLigo
  , baseDAOTreasuryStorageLigo
  , baseDAOLambdaStorageLigo
  , baseDAOLambdaregistryStorageLigo
  , baseDAOLambdatreasuryStorageLigo

  , getBaseDAOContract
  ) where

import Universum

import Data.Typeable (eqT, (:~:)(..))

import Lorentz qualified as L
import Morley.Michelson.Typed
import Test.Cleveland.Lorentz (embedContract)

import Ligo.BaseDAO.LambdaDAO.Types
import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.TreasuryDAO.Types
import Ligo.BaseDAO.Types
import Ligo.Util

getBaseDAOContract :: forall cep. (Typeable cep) => Contract (ToT (Parameter' (VariantToParam cep))) (ToT (StorageSkeleton (VariantToExtra cep)))
getBaseDAOContract = case eqT @cep @'Base of
  Just Refl -> baseDAOContractLigo
  Nothing -> case eqT @cep @'Registry of
    Just Refl -> baseDAORegistryLigo
    Nothing -> case eqT @cep @'Treasury of
      Just Refl -> baseDAOTreasuryLigo
      Nothing -> case eqT @cep @'Lambda of
        Just Refl -> baseDAOLambdaLigo
        Nothing -> error "Unknown contract"

baseDAOContractLigo :: Contract (ToT Parameter) (ToT Storage)
baseDAOContractLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' ()) @Storage @() "resources/trivialDAO.tz")

baseDAOLambdaRegistryLigo :: Contract (ToT (Parameter' (VariantToParam 'LambdaRegistry))) (ToT LambdaStorage)
baseDAOLambdaRegistryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' (VariantToParam 'LambdaRegistry)) @LambdaStorage @() "resources/lambdaregistryDAO.tz")

baseDAOLambdaTreasuryLigo :: Contract (ToT (Parameter' (VariantToParam 'LambdaTreasury))) (ToT LambdaStorage)
baseDAOLambdaTreasuryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' (VariantToParam 'LambdaTreasury)) @LambdaStorage @() "resources/lambdatreasuryDAO.tz")

baseDAORegistryLigo :: Contract (ToT (Parameter' RegistryCustomEpParam)) (ToT RegistryStorage)
baseDAORegistryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' RegistryCustomEpParam) @RegistryStorage @() "resources/registryDAO.tz")

baseDAOTreasuryLigo :: Contract (ToT (Parameter' TreasuryCustomEpParam)) (ToT TreasuryStorage)
baseDAOTreasuryLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' TreasuryCustomEpParam) @TreasuryStorage @() "resources/treasuryDAO.tz")

baseDAOLambdaLigo :: Contract (ToT (Parameter' LambdaCustomEpParam)) (ToT LambdaStorage)
baseDAOLambdaLigo = L.toMichelsonContract
  $$(embedContract @(Parameter' LambdaCustomEpParam) @LambdaStorage @() "resources/lambdaDAO.tz")

baseDAOLambdaregistryStorageLigo :: LambdaStorage
baseDAOLambdaregistryStorageLigo =
  fromVal ($(fetchValue @LambdaStorage "resources/lambdaregistryDAO_storage.tz"))

baseDAOLambdatreasuryStorageLigo :: LambdaStorage
baseDAOLambdatreasuryStorageLigo =
  fromVal ($(fetchValue @LambdaStorage "resources/lambdatreasuryDAO_storage.tz"))

baseDAOLambdaStorageLigo :: LambdaStorage
baseDAOLambdaStorageLigo =
  fromVal ($(fetchValue @LambdaStorage "resources/lambdaDAO_storage.tz"))

baseDAORegistryStorageLigo :: RegistryStorage
baseDAORegistryStorageLigo =
  fromVal ($(fetchValue @RegistryStorage "resources/registryDAO_storage.tz"))

baseDAOTreasuryStorageLigo :: TreasuryStorage
baseDAOTreasuryStorageLigo =
  fromVal ($(fetchValue @TreasuryStorage "resources/treasuryDAO_storage.tz"))
