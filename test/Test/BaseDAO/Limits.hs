-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Checks on that our contract fits into reasonable limits.
module Test.BaseDAO.Limits
  ( test_Operation_size
  ) where

import Universum

import Fmt (pretty)
import Named (defaults, (!))
import Test.HUnit (Assertion, assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz.Empty
import Lorentz.OpSize
import Lorentz.Value
import Tezos.Address

import qualified Lorentz.Contracts.BaseDAO as DAO
import qualified Lorentz.Contracts.TrivialDAO as DAO

originationOpSize :: _ => DAO.Config ce pm Empty -> DAO.Storage ce pm -> OpSize
originationOpSize config storage = mconcat
  [ OpSize 1601  -- heuristically evaluated base origination cost
  , contractOpSize $ DAO.baseDaoContract config
  , valueOpSize storage
  ]

dummyAddress :: Address
dummyAddress = unsafeParseAddress "tz1i6PKGNwVHd9wLKq4RrXU1RYFroAfqTM9z"

opSizeIsUnder :: OpSize -> OpSize -> Assertion
actual `opSizeIsUnder` limit =
  unless (actual <= limit) $
    assertFailure $ "Operation size is " <> pretty actual <> " \
                    \and that exceedes the limit " <> pretty limit

averageStorage :: DAO.Storage () ()
averageStorage =
  -- TODO: fill this someday
  DAO.mkStorage ! #admin dummyAddress ! #extra () ! #metadata mempty ! defaults

averageConfig :: DAO.Config () () Empty
averageConfig = DAO.trivialConfig
  -- TODO: fill this someday

test_Operation_size :: TestTree
test_Operation_size =
  testGroup "Contract code is small enough"
  [ testCase "Soft limit" $ do
      -- Here we check that code is small enough to fit into
      -- operation size limit during origination.
      --
      -- We leave some reserve, so that adding a bit more code
      -- and originating the contract with a bigger storage than expected
      -- should still succeed, thus you can temporarily disable this test.
      -- But make sure that work on optimizing the contract is planned.
      let opSize = originationOpSize @() averageConfig averageStorage
      opSize `opSizeIsUnder` OpSize 14000

  , testCase "Hard limit" $ do
      -- Failure of this check means that we cannot originate the contract
      -- with reasonably big storages anymore and optimizations have to be
      -- applied as fast as possible.
      let opSize = originationOpSize @() averageConfig averageStorage
      opSize `opSizeIsUnder` opSizeHardLimit
  ]
