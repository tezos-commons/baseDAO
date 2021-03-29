-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Token.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.BaseDAO.Types
import qualified Test.BaseDAO.FA2 as FA2
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ FA2.zeroTransferScenario originateTrivialDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ FA2.validTransferScenario originateTrivialDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ FA2.validateTokenScenario originateTrivialDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ FA2.emptyTransferListScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ FA2.lowBalanceScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ FA2.noSourceAccountScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ FA2.badOperatorScenario originateTrivialDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ FA2.noForeignMoneyScenario originateTrivialDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ FA2.validTransferOwnerScenario originateTrivialDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ FA2.updatingOperatorScenario originateTrivialDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ FA2.balanceOfOwnerScenario originateTrivialDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ FA2.validateTokenOwnerScenario originateTrivialDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ FA2.lowBalanceOwnerScenario originateTrivialDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ FA2.noForeignMoneyOwnerScenario originateTrivialDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ FA2.adminTransferScenario originateTrivialDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ FA2.adminTransferFrozenScenario $ originateTrivialDaoWithBalance
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  , testGroup "Entrypoints respect migration status"
      [ nettestScenario "transfer respects migration status"
          $ uncapsNettest $ FA2.transferAfterMigrationScenario originateTrivialDao
      , nettestScenario "update operator respects migration status "
          $ uncapsNettest $ FA2.updatingOperatorAfterMigrationScenario originateTrivialDao
      , nettestScenario "balanceOf request respects migration status"
          $ uncapsNettest $ FA2.balanceOfRequestAfterMigrationScenario originateTrivialDao
      ]
  ]
