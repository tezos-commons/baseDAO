-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Ligo.BaseDAO.TreasuryDAO.Types
  ( TreasuryCustomEpParam
  , TreasuryExtra (..)
  , TreasuryFullStorage
  ) where

import Universum hiding (fromInteger)

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Morley.Client

import Ligo.BaseDAO.Types

type instance VariantToParam 'Treasury = TreasuryCustomEpParam
type instance VariantToExtra 'Treasury = TreasuryExtra

type TreasuryFullStorage = FullStorageSkeleton (VariantToExtra 'Treasury)

type TreasuryCustomEpParam = ()

data TreasuryExtra = TreasuryExtra
  { teFrozenScaleValue :: Natural
  , teFrozenExtraValue :: Natural
  , teMaxProposalSize :: Natural
  , teSlashScaleValue :: Natural
  , teSlashDivisionValue :: Natural
  , teMinXtzAmount :: Mutez
  , teMaxXtzAmount :: Mutez
  } deriving stock (Eq)

instance Default TreasuryExtra where
  def = TreasuryExtra 1 0 1000 1 1 zeroMutez (toMutez 1)

instance Buildable TreasuryExtra where
  build = genericF

type instance AsRPC TreasuryExtra = TreasuryExtra

customGeneric "TreasuryExtra" ligoLayout
deriving anyclass instance IsoValue TreasuryExtra
instance HasAnnotation TreasuryExtra where
  annOptions = baseDaoAnnOptions
