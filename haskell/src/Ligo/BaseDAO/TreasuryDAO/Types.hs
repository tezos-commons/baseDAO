-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Ligo.BaseDAO.TreasuryDAO.Types
  ( TreasuryCustomEpParam
  , TreasuryExtra (..)
  , TreasuryFullStorage
  ) where

import Universum

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Morley.Client

import Ligo.BaseDAO.Types

type instance VariantToParam 'Treasury = TreasuryCustomEpParam
type instance VariantToExtra 'Treasury = TreasuryExtra

type TreasuryFullStorage = FullStorageSkeleton (VariantToExtra 'Treasury)

type TreasuryCustomEpParam = ()

data TreasuryExtra = TreasuryExtra
  { teFrozenScaleValue :: Maybe Natural
  , teFrozenExtraValue :: Maybe Natural
  , teMaxProposalSize :: Maybe Natural
  , teSlashScaleValue :: Maybe Natural
  , teSlashDivisionValue :: Maybe Natural
  , teMinXtzAmount :: Maybe Mutez
  , teMaxXtzAmount :: Maybe Mutez
  } deriving stock (Eq)

instance Default TreasuryExtra where
  def = TreasuryExtra Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Buildable TreasuryExtra where
  build = genericF

type instance AsRPC TreasuryExtra = TreasuryExtra

customGeneric "TreasuryExtra" ligoLayout
deriving anyclass instance IsoValue TreasuryExtra
instance HasAnnotation TreasuryExtra where
  annOptions = baseDaoAnnOptions
