-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Token.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Lorentz
import Lorentz.Test
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)
import Util.Named

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.BaseDAO.Management (expectMigrated)
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          zeroTransferScenario
      , nettestScenario "allows valid transfer and check balance"
          validTransferScenario
      , nettestScenario "validates token id"
          validateTokenScenario
      , nettestScenario "accepts an empty list of transfers"
          emptyTransferListScenario
      , nettestScenario "aborts if there is a failure (due to low balance)"
          lowBalanceScenario
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          noSourceAccountScenario
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          badOperatorScenario
      , nettestScenario "cannot transfer foreign money"
          noForeignMoneyScenario
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          validTransferOwnerScenario
      , nettestScenario "allows updating operator "
          updatingOperatorScenario
      , nettestScenario "allows balanceOf request"
          balanceOfOwnerScenario
      , nettestScenario "validates token id"
          validateTokenOwnerScenario
      , nettestScenario "aborts if there is a failure (due to low balance)"
          lowBalanceOwnerScenario
      , nettestScenario "cannot transfer foreign money"
          noForeignMoneyOwnerScenario
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        adminTransferScenario
    , nettestScenario "transfer frozen tokens"
        adminTransferFrozenScenario
    ]
  , testGroup "Entrypoints respect migration status"
      [ nettestScenario "transfer respects migration status"
          transferAfterMigrationScenario
      , nettestScenario "update operator respects migration status "
          updatingOperatorAfterMigrationScenario
      , nettestScenario "balanceOf request respects migration status"
          balanceOfRequestAfterMigrationScenario
      , nettestScenario "tokenMetadataRegistry request respects migration status"
          tokenMetadataRegistryRequestAfterMigrationScenario
      ]
  ]

zeroTransferScenario :: (Monad m) => NettestImpl m -> m ()
zeroTransferScenario = uncapsNettest $ do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateBaseDao @()
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
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao @()
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
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao @()
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
  ((owner1, _), (owner2, newAddress1), dao, admin) <- originateBaseDao @()
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
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao @()
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = owner2
        , opTokenId = 0
        }
      transferParams = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = 0
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator params]
  callFrom (AddressResolved owner2) dao (Call @"Transfer") transferParams

  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.RemoveOperator params]
  callFrom (AddressResolved owner2) dao (Call @"Transfer") transferParams
    & expectCustomError_ #fA2_NOT_OPERATOR

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
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((owner1, _), _, dao, _) <- originateBaseDao @()

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
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((_, op1), _, dao, _) <- originateBaseDao @()
  callFrom (AddressResolved op1) dao (Call @"Transfer") []

validateTokenScenario :: (Monad m) => NettestImpl m -> m ()
validateTokenScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDao @()

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
  ((owner1, _), _, dao, _) <- originateBaseDao @()
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
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDao @()

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
    <- originateBaseDaoWithBalance @() DAO.defaultConfig
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
  ((owner1, op1), (owner2, newAddress1), dao, admin) <- originateBaseDao @()

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
  ((owner1, newAddress1), _, dao, admin) <- originateBaseDao @()
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
  ((owner1, newAddress1), _, dao, admin) <- originateBaseDao @()
  consumer <- originateSimple "consumer" [] (contractConsumer @Address)

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()

  callFrom (AddressResolved owner1) dao (Call @"Token_metadata_registry") (toContractRef consumer)
    & expectMigrated newAddress1
