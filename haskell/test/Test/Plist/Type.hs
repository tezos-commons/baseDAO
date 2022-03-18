-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Plist.Type
  ( PlistParameter (..)
  , PlistStorage (..)
  ) where

import Universum hiding (drop, swap)

import Fmt (Buildable, build, genericF)

import Lorentz hiding (and, div, now, (>>))

import Ligo.BaseDAO.Types


data PlistParameter
  = Insert ProposalKey
  | Delete ProposalKey
  | Mem ProposalKey
  | Pop ()
  deriving stock (Eq, Show)

instance Buildable PlistParameter where
  build = genericF

customGeneric "PlistParameter" ligoLayout
deriving anyclass instance IsoValue PlistParameter
instance ParameterHasEntrypoints PlistParameter where
  type ParameterEntrypointsDerivation PlistParameter = EpdDelegate

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
instance Buildable PlistStorage where
  build = genericF
