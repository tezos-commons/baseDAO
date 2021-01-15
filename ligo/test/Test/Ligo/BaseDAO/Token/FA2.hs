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
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ Share.zeroTransferScenario originateLigoDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferScenario originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenScenario originateLigoDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ Share.emptyTransferListScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ Share.noSourceAccountScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ Share.badOperatorScenario originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyScenario originateLigoDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferOwnerScenario originateLigoDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ Share.updatingOperatorScenario originateLigoDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ Share.balanceOfOwnerScenario originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenOwnerScenario originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceOwnerScenario originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyOwnerScenario originateLigoDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ Share.adminTransferScenario originateLigoDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ Share.adminTransferFrozenScenario $ originateLigoDaoWithBalance
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  -- TODO #100: Migration not implemented
  -- , testGroup "Entrypoints respect migration status"
  --     [ nettestScenario "transfer respects migration status"
  --         $ uncapsNettest $ Share.transferAfterMigrationScenario originateLigoDao
  --     , nettestScenario "update operator respects migration status "
  --         $ uncapsNettest $ Share.updatingOperatorAfterMigrationScenario originateLigoDao
  --     , nettestScenario "balanceOf request respects migration status"
  --         $ uncapsNettest $ Share.balanceOfRequestAfterMigrationScenario originateLigoDao
  --     , nettestScenario "tokenMetadataRegistry request respects migration status"
  --         $ uncapsNettest $ Share.tokenMetadataRegistryRequestAfterMigrationScenario originateLigoDao
  --     ]
  ]

