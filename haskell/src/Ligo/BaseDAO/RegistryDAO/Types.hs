-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Ligo.BaseDAO.RegistryDAO.Types
  ( RegistryCustomEpParam (..)
  , RegistryExtra (..)
  , RegistryFullStorage
  , LookupRegistryParam (..)
  ) where

import Universum

import qualified Data.Set as S
import qualified Data.Map as M

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Morley.Client

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
  , reFrozenScaleValue :: Maybe Natural
  , reFrozenExtraValue :: Maybe Natural
  , reMaxProposalSize :: Maybe Natural
  , reSlashScaleValue :: Maybe Natural
  , reSlashDivisionValue :: Maybe Natural
  , reMinXtzAmount :: Maybe Mutez
  , reMaxXtzAmount :: Maybe Mutez
  } deriving stock (Eq)

instance Default RegistryExtra where
  def = RegistryExtra M.empty M.empty S.empty Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Buildable RegistryExtra where
  build = genericF

type instance AsRPC RegistryExtra = RegistryExtra

customGeneric "RegistryExtra" ligoLayout
deriving anyclass instance IsoValue RegistryExtra
instance HasAnnotation RegistryExtra where
  annOptions = baseDaoAnnOptions

type instance VariantToParam 'Registry = RegistryCustomEpParam
type instance VariantToExtra 'Registry = RegistryExtra

type RegistryFullStorage = FullStorageSkeleton (VariantToExtra 'Registry)
