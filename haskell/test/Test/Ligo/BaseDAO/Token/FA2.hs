-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)

import qualified Test.Ligo.BaseDAO.FA2 as FA2

import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ FA2.zeroTransferScenario originateLigoDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ FA2.validTransferScenario originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ FA2.validateTokenScenario
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ FA2.emptyTransferListScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ FA2.lowBalanceScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ FA2.noSourceAccountScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ FA2.badOperatorScenario originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ FA2.noForeignMoneyScenario
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ FA2.validTransferOwnerScenario
      , nettestScenario "allows updating operator"
          $ uncapsNettest $ FA2.updatingOperatorScenario
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ FA2.balanceOfOwnerScenario originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ FA2.validateTokenOwnerScenario
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ FA2.lowBalanceOwnerScenario originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ FA2.noForeignMoneyOwnerScenario
      ]
  , testGroup "Admin:"
    [ nettestScenario "can transfer frozen tokens from any address to any address"
        $ uncapsNettest $ FA2.adminTransferScenario originateLigoDao
    , nettestScenario "cannot transfer unfrozen tokens from any address to any address"
        $ uncapsNettest $ FA2.prohibitedAdminTransferScenario
    ]
  ]
