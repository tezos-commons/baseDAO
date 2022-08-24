-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Ligo.BaseDAO.TreasuryDAO.Types
  ( TreasuryCustomEpParam
  , TreasuryExtra (..)
  , TreasuryStorage
  ) where

import Universum hiding (fromInteger)

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Morley.AsRPC

import Ligo.BaseDAO.Types

type instance VariantToParam 'Treasury = TreasuryCustomEpParam
type instance VariantToExtra 'Treasury = TreasuryExtra

type TreasuryStorage = StorageSkeleton (VariantToExtra 'Treasury)

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
  def = TreasuryExtra 1 0 1000 1 1 zeroMutez [tz|1u|]

customGeneric "TreasuryExtra" ligoLayout
deriving anyclass instance IsoValue TreasuryExtra
instance HasAnnotation TreasuryExtra where
  annOptions = baseDaoAnnOptions

instance Buildable TreasuryExtra where
  build = genericF

instance HasRPCRepr TreasuryExtra where
  type AsRPC TreasuryExtra = TreasuryExtra

