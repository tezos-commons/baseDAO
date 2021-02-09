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

import BaseDAO.ShareTest.Common (OriginateFn)
import qualified BaseDAO.ShareTest.FA2 as Share
import qualified Ligo.BaseDAO.Types as Ligo

import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ Share.zeroTransferScenario startedUpLigoDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferScenario startedUpLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenScenario startedUpLigoDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ Share.emptyTransferListScenario startedUpLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceScenario startedUpLigoDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ Share.noSourceAccountScenario startedUpLigoDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ Share.badOperatorScenario startedUpLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyScenario startedUpLigoDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferOwnerScenario startedUpLigoDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ Share.updatingOperatorScenario startedUpLigoDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ Share.balanceOfOwnerScenario startedUpLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenOwnerScenario startedUpLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceOwnerScenario startedUpLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyOwnerScenario startedUpLigoDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ Share.adminTransferScenario startedUpLigoDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ Share.adminTransferFrozenScenario $ withDefaultStartup
        $ originateLigoDaoWithBalance [] Ligo.dynRecUnsafe Ligo.defaultConfigL
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  , testGroup "Entrypoints respect migration status"
      [ nettestScenario "transfer respects migration status"
          $ uncapsNettest $ Share.transferAfterMigrationScenario startedUpLigoDao
      , nettestScenario "update operator respects migration status "
          $ uncapsNettest $ Share.updatingOperatorAfterMigrationScenario startedUpLigoDao
      , nettestScenario "balanceOf request respects migration status"
          $ uncapsNettest $ Share.balanceOfRequestAfterMigrationScenario startedUpLigoDao
      ]
  ]
  where
    startedUpLigoDao :: MonadNettest caps base m => OriginateFn Ligo.ParameterL m
    startedUpLigoDao = withDefaultStartup originateLigoDao
