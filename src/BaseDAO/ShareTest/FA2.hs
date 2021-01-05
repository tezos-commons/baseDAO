-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains shared FA2 tests for testing Lorentz
-- and Ligo contracts.
module BaseDAO.ShareTest.FA2
  ( zeroTransferScenario
  , validTransferScenario
  , validateTokenScenario
  , emptyTransferListScenario
  , lowBalanceScenario
  , noSourceAccountScenario
  , badOperatorScenario
  , noForeignMoneyScenario
  , validTransferOwnerScenario
  , updatingOperatorScenario
  , balanceOfOwnerScenario
  , validateTokenOwnerScenario
  , lowBalanceOwnerScenario
  , noForeignMoneyOwnerScenario
  , adminTransferScenario
  , adminTransferFrozenScenario
  , transferAfterMigrationScenario
  , updatingOperatorAfterMigrationScenario
  , balanceOfRequestAfterMigrationScenario
  , tokenMetadataRegistryRequestAfterMigrationScenario
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test
import Morley.Nettest
import Util.Named

import BaseDAO.ShareTest.Common
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

zeroTransferScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
zeroTransferScenario _ originateFn = do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = nonexistent
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 0
            } ]
        } ]

  callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params

validTransferScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
validTransferScenario _ originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params

  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved owner2) dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = unfrozenTokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

validTransferOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
validTransferOwnerScenario _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]

  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved owner2) dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = unfrozenTokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

updatingOperatorAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, ParameterC param pm)
  => IsLorentz -> OriginateFn param m -> m ()
updatingOperatorAfterMigrationScenario isLorentz originateFn = do
  ((owner1, _), (owner2, newAddress1), dao, admin) <- originateFn
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = owner2
        , opTokenId = unfrozenTokenId
        }

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()
  case isLorentz of
    True ->
      callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator params]
        & expectCustomError #mIGRATED newAddress1
    False ->
      callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator params]
        & expectFailed (toAddress dao) [mt|MIGRATED|]

updatingOperatorScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
updatingOperatorScenario isLorentz originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = owner2
        , opTokenId = unfrozenTokenId
        }
      transferParams = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator params]
  callFrom (AddressResolved owner2) dao (Call @"Transfer") transferParams

  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.RemoveOperator params]

  let notOwnerParams = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = owner1
        , opTokenId = unfrozenTokenId
        }

  case isLorentz of
    True -> do
      callFrom (AddressResolved owner2) dao (Call @"Transfer") transferParams
        & expectCustomError_ #fA2_NOT_OPERATOR
      callFrom (AddressResolved owner1) dao (Call @"Update_operators") ([FA2.AddOperator notOwnerParams])
        & expectCustomError_ #nOT_OWNER

      callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.RemoveOperator notOwnerParams]
        & expectCustomError_ #nOT_OWNER

    False -> do
      callFrom (AddressResolved owner2) dao (Call @"Transfer") transferParams
        & expectFailed (toAddress dao) [mt|FA2_NOT_OPERATOR|]
      callFrom (AddressResolved owner1) dao (Call @"Update_operators") ([FA2.AddOperator notOwnerParams])
        & expectFailed (toAddress dao) [mt|NOT_OWNER|]
      callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.RemoveOperator notOwnerParams]
        & expectFailed (toAddress dao) [mt|NOT_OWNER|]

lowBalanceScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
lowBalanceScenario isLorentz originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 200
            } ]
        } ]
  case isLorentz of
    True ->
      callFrom (AddressResolved op1) dao (Call @"Transfer") params
        & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)
    False ->
      callFrom (AddressResolved op1) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|FA2_INSUFFICIENT_BALANCE|]

lowBalanceOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
lowBalanceOwnerScenario isLorentz originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 200
            } ]
        } ]
  case isLorentz of
    True ->
      callFrom (AddressResolved owner1) dao (Call @"Transfer") params
        & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)
    False ->
      callFrom (AddressResolved owner1) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|FA2_INSUFFICIENT_BALANCE|]

noSourceAccountScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
noSourceAccountScenario isLorentz originateFn = do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = nonexistent
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 1
            } ]
        } ]

  -- Failed with 'fA2_INSUFFICIENT_BALANCE' due to nonexistent account is treated
  -- as an existing account with 0 balance.
  case isLorentz of
    True ->
      callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params
        & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0)
    False ->
      callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|FA2_INSUFFICIENT_BALANCE|]

badOperatorScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
badOperatorScenario isLorentz originateFn = do
  op3  :: Address <- newAddress "operator3"
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 0
            } ]
        } ]
  case isLorentz of
    True ->
      callFrom (AddressResolved op3) dao (Call @"Transfer") params
        & expectCustomError_ #fA2_NOT_OPERATOR
    False ->
      callFrom (AddressResolved op3) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|FA2_NOT_OPERATOR|]

emptyTransferListScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
emptyTransferListScenario _ originateFn = do
  ((_, op1), _, dao, _) <- originateFn
  callFrom (AddressResolved op1) dao (Call @"Transfer") []

unknownTokenId :: FA2.TokenId
unknownTokenId = FA2.TokenId 2

validateTokenScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
validateTokenScenario isLorentz originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith = callFrom (AddressResolved op1) dao (Call @"Transfer")

  callWith (params unfrozenTokenId)

  case isLorentz of
    True -> do
      callWith (params frozenTokenId)
        & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
      callWith (params unknownTokenId)
        & expectCustomError_ #fA2_TOKEN_UNDEFINED
    False -> do
      callWith (params frozenTokenId)
        & expectFailed (toAddress dao) [mt|FROZEN_TOKEN_NOT_TRANSFERABLE|]
      callWith (params unknownTokenId)
        & expectFailed (toAddress dao) [mt|FA2_TOKEN_UNDEFINED|]

validateTokenOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
validateTokenOwnerScenario isLorentz originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith = callFrom (AddressResolved owner1) dao (Call @"Transfer")

  callWith (params unfrozenTokenId)

  case isLorentz of
    True -> do
      callWith (params frozenTokenId)
        & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
      callWith (params unknownTokenId)
        & expectCustomError_ #fA2_TOKEN_UNDEFINED
    False -> do
      callWith (params frozenTokenId)
        & expectFailed (toAddress dao) [mt|FROZEN_TOKEN_NOT_TRANSFERABLE|]
      callWith (params unknownTokenId)
        & expectFailed (toAddress dao) [mt|FA2_TOKEN_UNDEFINED|]


noForeignMoneyScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
noForeignMoneyScenario isLorentz originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  case isLorentz of
    True ->
      callFrom (AddressResolved op1) dao (Call @"Transfer") params
        & expectCustomError_ #fA2_NOT_OPERATOR
    False ->
       callFrom (AddressResolved op1) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|FA2_NOT_OPERATOR|]

noForeignMoneyOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
noForeignMoneyOwnerScenario isLorentz originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  case isLorentz of
    True ->
      callFrom (AddressResolved owner1) dao (Call @"Transfer") params
        & expectCustomError_ #fA2_NOT_OPERATOR
    False ->
       callFrom (AddressResolved owner1) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|FA2_NOT_OPERATOR|]

balanceOfOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
balanceOfOwnerScenario isLorentz originateFn = do
  ((owner1, _), _, dao, _) <- originateFn
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith = callFrom (AddressResolved owner1) dao (Call @"Balance_of")

  callWith (params [ FA2.BalanceRequestItem owner1 unfrozenTokenId ])
  callWith (params [ FA2.BalanceRequestItem owner1 frozenTokenId ])
  callWith (params [])
  case isLorentz of
    True ->
      callWith (params [ FA2.BalanceRequestItem owner1 unknownTokenId ])
        & expectCustomError_ #fA2_TOKEN_UNDEFINED
    False ->
      callWith (params [ FA2.BalanceRequestItem owner1 unknownTokenId ])
        & expectFailed (toAddress dao) [mt|FA2_TOKEN_UNDEFINED|]

adminTransferScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
adminTransferScenario _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved admin) dao (Call @"Transfer") params


adminTransferFrozenScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => IsLorentz -> OriginateFn param m -> m ()
adminTransferFrozenScenario _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = frozenTokenId
            , tdAmount = 10
            } ]
        } ]
  callFrom (AddressResolved admin) dao (Call @"Transfer") params

transferAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, ParameterC param pm)
  => IsLorentz -> OriginateFn param m -> m ()
transferAfterMigrationScenario isLorentz originateFn = do
  ((owner1, op1), (owner2, newAddress1), dao, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()
  case isLorentz of
    True ->
      callFrom (AddressResolved op1) dao (Call @"Transfer") params
        & expectMigrated newAddress1
    False ->
      callFrom (AddressResolved op1) dao (Call @"Transfer") params
        & expectFailed (toAddress dao) [mt|MIGRATED|]

balanceOfRequestAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, ParameterC param pm)
  => IsLorentz -> OriginateFn param m -> m ()
balanceOfRequestAfterMigrationScenario _ originateFn = do
  ((owner1, newAddress1), _, dao, admin) <- originateFn
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith = callFrom (AddressResolved owner1) dao (Call @"Balance_of")

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()

  callWith (params [ FA2.BalanceRequestItem owner1 unfrozenTokenId ])
    & expectMigrated newAddress1

tokenMetadataRegistryRequestAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, ParameterC param pm)
  => IsLorentz -> OriginateFn param m -> m ()
tokenMetadataRegistryRequestAfterMigrationScenario _ originateFn = do
  ((owner1, newAddress1), _, dao, admin) <- originateFn
  consumer <- originateSimple "consumer" [] (contractConsumer @Address)

  callFrom (AddressResolved admin) dao (Call @"Migrate") (#newAddress .! newAddress1)
  callFrom (AddressResolved newAddress1) dao (Call @"Confirm_migration") ()

  callFrom (AddressResolved owner1) dao (Call @"Token_metadata_registry") (toContractRef consumer)
    & expectMigrated newAddress1
