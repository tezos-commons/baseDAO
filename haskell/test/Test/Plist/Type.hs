-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Plist.Type
  ( PlistParameter (..)
  , PlistStorage (..)
  ) where

import Universum hiding (drop, swap)

import Fmt (Buildable, GenericBuildable(..))

import Lorentz hiding (and, div, now, (>>))

import Ligo.BaseDAO.Types


data PlistParameter
  = Insert ProposalKey
  | Delete ProposalKey
  | Mem ProposalKey
  | Pop ()
  deriving stock (Eq, Show)

customGeneric "PlistParameter" ligoLayout
deriving anyclass instance IsoValue PlistParameter
instance ParameterHasEntrypoints PlistParameter where
  type ParameterEntrypointsDerivation PlistParameter = EpdDelegate

deriving via GenericBuildable PlistParameter instance Buildable PlistParameter

data PlistStorage = PlistStorage
  { psPlist :: (Maybe ProposalDoublyLinkedList)
  , psMemResult :: Bool
  , psPopResult :: Maybe ProposalKey
  }

customGeneric "PlistStorage" ligoLayout

deriving stock instance Show PlistStorage
deriving stock instance Eq PlistStorage
deriving anyclass instance IsoValue PlistStorage
instance HasAnnotation PlistStorage where
  annOptions = baseDaoAnnOptions
deriving via GenericBuildable PlistStorage instance Buildable PlistStorage
