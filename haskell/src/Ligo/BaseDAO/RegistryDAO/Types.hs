-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Ligo.BaseDAO.RegistryDAO.Types
  ( RegistryCustomEpParam (..)
  , LookupRegistryParam (..)
  ) where

import Universum

import Fmt (Buildable, build, genericF)
import Lorentz as L

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

type instance VariantToParam 'Registry = RegistryCustomEpParam

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
