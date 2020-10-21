-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Lorentz
import Lorentz.Test
import Morley.Nettest
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util.Named

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Test.BaseDAO.Management (expectMigrated)
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ testCase "allows zero transfer from non-existent operator" $
          nettestTestExpectation zeroTransferScenario
      , testCase "allows valid transfer and check balance" $
          nettestTestExpectation validTransferScenario
      , testCase "validates token id" $
          nettestTestExpectation validateTokenScenario
      , testCase "accepts an empty list of transfers" $
          nettestTestExpectation emptyTransferListScenario
      , testCase "aborts if there is a failure (due to low balance)" $
          nettestTestExpectation lowBalanceScenario
      , testCase "aborts if there is a failure (due to non existent source account)" $
          nettestTestExpectation noSourceAccountScenario
      , testCase "aborts if there is a failure (due to bad operator)" $
          nettestTestExpectation badOperatorScenario
      , testCase "cannot transfer foreign money" $
          nettestTestExpectation noForeignMoneyScenario
      ]
  , testGroup "Owner:"
      [ testCase "allows valid transfer and check balance" $
          nettestTestExpectation validTransferOwnerScenario
      , testCase "allows updating operator " $
          nettestTestExpectation updatingOperatorScenario
      , testCase "allows balanceOf request" $
          nettestTestExpectation balanceOfOwnerScenario
      , testCase "validates token id" $
          nettestTestExpectation validateTokenOwnerScenario
      , testCase "aborts if there is a failure (due to low balance)" $
          nettestTestExpectation lowBalanceOwnerScenario
      , testCase "cannot transfer foreign money" $
          nettestTestExpectation noForeignMoneyOwnerScenario
      ]
  , testGroup "Admin:"
    [ testCase "transfer tokens from any address to any address" $
        nettestTestExpectation adminTransferScenario
    , testCase "transfer frozen tokens" $
        nettestTestExpectation adminTransferFrozenScenario
    ]
  , testGroup "Entrypoints respect migration status"
      [ testCase "transfer respects migration status" $
          nettestTestExpectation transferAfterMigrationScenario
      , testCase "update operator respects migration status " $
          nettestTestExpectation updatingOperatorAfterMigrationScenario
      , testCase "balanceOf request respects migration status" $
          nettestTestExpectation balanceOfRequestAfterMigrationScenario
      , testCase "tokenMetadataRegistry request respects migration status" $
          nettestTestExpectation tokenMetadataRegistryRequestAfterMigrationScenario
      ]
  ]

zeroTransferScenario :: (Monad m) => NettestImpl m -> m ()
zeroTransferScenario = uncapsNettest $ do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateBaseDao
  let params = [ FA2.TransferItem
        { tiFrom = nonexistent
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 0
            } ]
        } ]

  callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params

validTransferScenario :: (Monad m) => NettestImpl m -> m ()
validTransferScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params

  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved owner2) dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = 0
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

validTransferOwnerScenario :: (Monad m) => NettestImpl m -> m ()
validTransferOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]

  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved owner2) dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = 0
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

updatingOperatorAfterMigrationScenario :: (Monad m) => NettestImpl m -> m ()
updatingOperatorAfterMigrationScenario = uncapsNettest $ do
  ((owner1, _), (owner2, newAddress1), dao, admin) <- originateBaseDao
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = owner2
        , opTokenId = 0
        }

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator params]
    & expectCustomError #mIGRATED newAddress1

updatingOperatorScenario :: (Monad m) => NettestImpl m -> m ()
updatingOperatorScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = owner2
        , opTokenId = 0
        }
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator params]
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.RemoveOperator params]

  let notOwnerParams = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = owner1
        , opTokenId = 0
        }

  callFrom (AddressResolved owner1) dao (Call @"Update_operators") ([FA2.AddOperator notOwnerParams])
    & expectCustomError_ #nOT_OWNER

  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.RemoveOperator notOwnerParams]
    & expectCustomError_ #nOT_OWNER

lowBalanceScenario :: (Monad m) => NettestImpl m -> m ()
lowBalanceScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = 0
            , tdAmount = 200
            } ]
        } ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)

lowBalanceOwnerScenario :: (Monad m) => NettestImpl m -> m ()
lowBalanceOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = 0
            , tdAmount = 200
            } ]
        } ]
  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)

noSourceAccountScenario :: (Monad m) => NettestImpl m -> m ()
noSourceAccountScenario = uncapsNettest $ do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = nonexistent
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 1
            } ]
        } ]

  -- Failed with 'fA2_INSUFFICIENT_BALANCE' due to nonexistent account is treated
  -- as an existing account with 0 balance.
  callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0)

badOperatorScenario :: (Monad m) => NettestImpl m -> m ()
badOperatorScenario = uncapsNettest $ do
  op3  :: Address <- newAddress "operator3"
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 0
            } ]
        } ]
  callFrom (AddressResolved op3) dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

emptyTransferListScenario :: (Monad m) => NettestImpl m -> m ()
emptyTransferListScenario = uncapsNettest $ do
  ((_, op1), _, dao, _) <- originateBaseDao
  callFrom (AddressResolved op1) dao (Call @"Transfer") []

validateTokenScenario :: (Monad m) => NettestImpl m -> m ()
validateTokenScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith = callFrom (AddressResolved op1) dao (Call @"Transfer")

  callWith (params 0)
  callWith (params 1)
    & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
  callWith (params 2)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

validateTokenOwnerScenario :: (Monad m) => NettestImpl m -> m ()
validateTokenOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith = callFrom (AddressResolved owner1) dao (Call @"Transfer")

  callWith (params 0)
  callWith (params 1)
    & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
  callWith (params 2)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

noForeignMoneyScenario :: (Monad m) => NettestImpl m -> m ()
noForeignMoneyScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

noForeignMoneyOwnerScenario :: (Monad m) => NettestImpl m -> m ()
noForeignMoneyOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

balanceOfOwnerScenario :: (Monad m) => NettestImpl m -> m ()
balanceOfOwnerScenario = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDao
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith = callFrom (AddressResolved owner1) dao (Call @"Balance_of")

  callWith (params [ FA2.BalanceRequestItem owner1 0 ])
  callWith (params [ FA2.BalanceRequestItem owner1 1 ])
  callWith (params [])
  callWith (params [ FA2.BalanceRequestItem owner1 2 ])
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

adminTransferScenario :: (Monad m) => NettestImpl m -> m ()
adminTransferScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved admin) dao (Call @"Transfer") params


adminTransferFrozenScenario :: (Monad m) => NettestImpl m -> m ()
adminTransferFrozenScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateBaseDaoWithBalance DAO.defaultConfig
        (\owner1 owner2 ->
            [ ((owner1, 1), 100)
            , ((owner2, 1), 100)
            ]
        )

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 1
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved admin) dao (Call @"Transfer") params

transferAfterMigrationScenario :: (Monad m) => NettestImpl m -> m ()
transferAfterMigrationScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, newAddress1), dao, admin) <- originateBaseDao

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()
  callFrom (AddressResolved op1) dao (Call @"Transfer") params
    & expectMigrated newAddress1

balanceOfRequestAfterMigrationScenario :: (Monad m) => NettestImpl m -> m ()
balanceOfRequestAfterMigrationScenario = uncapsNettest $ do
  ((owner1, newAddress1), _, dao, admin) <- originateBaseDao
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith = callFrom (AddressResolved owner1) dao (Call @"Balance_of")

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()

  callWith (params [ FA2.BalanceRequestItem owner1 0 ])
    & expectMigrated newAddress1

tokenMetadataRegistryRequestAfterMigrationScenario :: (Monad m) => NettestImpl m -> m ()
tokenMetadataRegistryRequestAfterMigrationScenario = uncapsNettest $ do
  ((owner1, newAddress1), _, dao, admin) <- originateBaseDao
  consumer <- originateSimple "consumer" [] (contractConsumer @Address)

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()

  callFrom (AddressResolved owner1) dao (Call @"Token_metadata_registry") (toContractRef consumer)
    & expectMigrated newAddress1
