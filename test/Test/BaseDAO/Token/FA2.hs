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
          $ uncapsNettest $ Share.zeroTransferScenario True originateTrivialDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferScenario True originateTrivialDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenScenario True originateTrivialDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ Share.emptyTransferListScenario True originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceScenario True originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ Share.noSourceAccountScenario True originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ Share.badOperatorScenario True originateTrivialDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyScenario True originateTrivialDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferOwnerScenario True originateTrivialDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ Share.updatingOperatorScenario True originateTrivialDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ Share.balanceOfOwnerScenario True originateTrivialDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenOwnerScenario True originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceOwnerScenario True originateTrivialDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyOwnerScenario True originateTrivialDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ Share.adminTransferScenario True originateTrivialDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ Share.adminTransferFrozenScenario True $ originateTrivialDaoWithBalance
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  , testGroup "Entrypoints respect migration status"
      [ nettestScenario "transfer respects migration status"
          $ uncapsNettest $ Share.transferAfterMigrationScenario True originateTrivialDao
      , nettestScenario "update operator respects migration status "
          $ uncapsNettest $ Share.updatingOperatorAfterMigrationScenario True originateTrivialDao
      , nettestScenario "balanceOf request respects migration status"
          $ uncapsNettest $ Share.balanceOfRequestAfterMigrationScenario True originateTrivialDao
      , nettestScenario "tokenMetadataRegistry request respects migration status"
          $ uncapsNettest $ Share.tokenMetadataRegistryRequestAfterMigrationScenario True originateTrivialDao
      ]
  ]
