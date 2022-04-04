-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Ligo.BaseDAO.RegistryDAO.Types
  ( RegistryCustomEpParam (..)
  , RegistryExtra (..)
  , RegistryFullStorage
  , LookupRegistryParam (..)
  ) where

import Universum hiding (fromInteger)

import Data.Map qualified as M
import Data.Set qualified as S

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Morley.AsRPC

import Ligo.BaseDAO.Types

data LookupRegistryParam = LookupRegistryParam
  { lkKey :: MText
  , lkCallback :: Address
  }

customGeneric "LookupRegistryParam" ligoCombLayout
deriving anyclass instance IsoValue LookupRegistryParam

instance Buildable LookupRegistryParam where
  build = genericF

instance HasAnnotation LookupRegistryParam where
  annOptions = baseDaoAnnOptions

data RegistryCustomEpParam
  = Lookup_registry LookupRegistryParam
  | RegistryCepDummy ()

instance Buildable RegistryCustomEpParam where
  build = genericF

customGeneric "RegistryCustomEpParam" ligoLayout
deriving anyclass instance IsoValue RegistryCustomEpParam

instance HasAnnotation RegistryCustomEpParam where
  annOptions = baseDaoAnnOptions

instance ParameterHasEntrypoints RegistryCustomEpParam where
  type ParameterEntrypointsDerivation RegistryCustomEpParam = EpdDelegate

instance ParameterHasEntrypoints (AllowXTZParam RegistryCustomEpParam) where
  type ParameterEntrypointsDerivation (AllowXTZParam RegistryCustomEpParam) = EpdDelegate

instance ParameterHasEntrypoints (Parameter' RegistryCustomEpParam) where
  type ParameterEntrypointsDerivation (Parameter' RegistryCustomEpParam) = EpdDelegate

type RegistryKey = MText
type RegistryValue = MText

data RegistryExtra = RegistryExtra
  { reRegistry :: Map RegistryKey RegistryValue
  , reRegistryAffected :: Map RegistryKey ProposalKey
  , reProposalReceivers :: Set Address
  , reFrozenScaleValue :: Natural
  , reFrozenExtraValue :: Natural
  , reMaxProposalSize :: Natural
  , reSlashScaleValue :: Natural
  , reSlashDivisionValue :: Natural
  , reMinXtzAmount :: Mutez
  , reMaxXtzAmount :: Mutez
  } deriving stock (Eq)

instance Default RegistryExtra where
  def = RegistryExtra M.empty M.empty S.empty 1 0 1000 1 1 zeroMutez [tz|1u|]

instance Buildable RegistryExtra where
  build = genericF

instance HasRPCRepr RegistryExtra where
  type AsRPC RegistryExtra = RegistryExtra

customGeneric "RegistryExtra" ligoLayout
deriving anyclass instance IsoValue RegistryExtra
instance HasAnnotation RegistryExtra where
  annOptions = baseDaoAnnOptions

type instance VariantToParam 'Registry = RegistryCustomEpParam
type instance VariantToExtra 'Registry = RegistryExtra

type RegistryFullStorage = FullStorageSkeleton (VariantToExtra 'Registry)
