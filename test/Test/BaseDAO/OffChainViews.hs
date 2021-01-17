-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import Named (defaults, (!))
import Test.Tasty (TestTree)

import Tezos.Address

import BaseDAO.ShareTest.OffChainViews
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.BaseDAO.TZIP16Metadata as DAO

defaultStorage :: DAO.Storage () ()
defaultStorage =
  DAO.mkStorage
  ! #admin (unsafeParseAddress "tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE")
  ! #extra ()
  ! #metadata mempty
  ! defaults

test_FA2 :: TestTree
test_FA2 =
  mkFA2Tests defaultStorage (DAO.mkMetadataSettings DAO.defaultMetadataConfig)
