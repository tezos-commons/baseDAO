-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Token.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)

import qualified BaseDAO.ShareTest.FA2 as Share
import Lorentz.Contracts.BaseDAO.Types
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ Share.zeroTransferScenario originateTrivialDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferScenario originateTrivialDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenScenario originateTrivialDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ Share.emptyTransferListScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ Share.noSourceAccountScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ Share.badOperatorScenario originateTrivialDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyScenario originateTrivialDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferOwnerScenario originateTrivialDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ Share.updatingOperatorScenario originateTrivialDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ Share.balanceOfOwnerScenario originateTrivialDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenOwnerScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceOwnerScenario originateTrivialDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyOwnerScenario originateTrivialDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ Share.adminTransferScenario originateTrivialDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ Share.adminTransferFrozenScenario $ originateTrivialDaoWithBalance
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  , testGroup "Entrypoints respect migration status"
      [ nettestScenario "transfer respects migration status"
          $ uncapsNettest $ Share.transferAfterMigrationScenario originateTrivialDao
      , nettestScenario "update operator respects migration status "
          $ uncapsNettest $ Share.updatingOperatorAfterMigrationScenario originateTrivialDao
      , nettestScenario "balanceOf request respects migration status"
          $ uncapsNettest $ Share.balanceOfRequestAfterMigrationScenario originateTrivialDao
      , nettestScenario "tokenMetadataRegistry request respects migration status"
          $ uncapsNettest $ Share.tokenMetadataRegistryRequestAfterMigrationScenario originateTrivialDao
      ]
  ]
