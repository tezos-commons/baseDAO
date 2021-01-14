-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.BaseDAO.Types

import qualified BaseDAO.ShareTest.FA2 as Share
import qualified Ligo.BaseDAO.ConfigDesc as Ligo
import qualified Ligo.BaseDAO.Helper as Ligo

import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ Share.zeroTransferScenario False originateLigoDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferScenario False originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenScenario False originateLigoDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ Share.emptyTransferListScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ Share.noSourceAccountScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ Share.badOperatorScenario False originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyScenario False originateLigoDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferOwnerScenario False originateLigoDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ Share.updatingOperatorScenario False originateLigoDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ Share.balanceOfOwnerScenario False originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenOwnerScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceOwnerScenario False originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyOwnerScenario False originateLigoDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ Share.adminTransferScenario False originateLigoDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ Share.adminTransferFrozenScenario False
        $ originateLigoDaoWithBalance Ligo.dynRecUnsafe (Ligo.ConfigDesc ())
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  -- TODO #100: Migration not implemented
  -- , testGroup "Entrypoints respect migration status"
  --     [ nettestScenario "transfer respects migration status"
  --         $ uncapsNettest $ Share.transferAfterMigrationScenario False originateLigoDao
  --     , nettestScenario "update operator respects migration status "
  --         $ uncapsNettest $ Share.updatingOperatorAfterMigrationScenario False originateLigoDao
  --     , nettestScenario "balanceOf request respects migration status"
  --         $ uncapsNettest $ Share.balanceOfRequestAfterMigrationScenario False originateLigoDao
  --     , nettestScenario "tokenMetadataRegistry request respects migration status"
  --         $ uncapsNettest $ Share.tokenMetadataRegistryRequestAfterMigrationScenario False originateLigoDao
  --     ]
  ]
