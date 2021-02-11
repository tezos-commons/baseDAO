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
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test hiding (withSender)
import Morley.Nettest
import Util.Named

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Management (expectMigrated)
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

zeroTransferScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
zeroTransferScenario originateFn = do
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

  withSender (AddressResolved nonexistent) $
    call dao (Call @"Transfer") params

validTransferScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
validTransferScenario originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved op1) $ call
    dao (Call @"Transfer") params

  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  withSender (AddressResolved owner2) $ call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = unfrozenTokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

validTransferOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
validTransferOwnerScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]

  withSender (AddressResolved owner1) $ call dao (Call @"Transfer") params
  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  withSender (AddressResolved owner2) $ call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = unfrozenTokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

updatingOperatorAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, ParameterC param pm)
  => OriginateFn param m -> m ()
updatingOperatorAfterMigrationScenario originateFn = do
  ((owner1, _), (owner2, newAddress1), dao, admin) <- originateFn
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = owner2
        , opTokenId = unfrozenTokenId
        }

  withSender (AddressResolved admin) $
    call dao (Call @"Migrate") (#newAddress .! newAddress1)
  withSender (AddressResolved newAddress1) $
    call dao (Call @"Confirm_migration") ()
  withSender (AddressResolved owner1) $
    call dao (Call @"Update_operators") [FA2.AddOperator params]
    & expectCustomError #mIGRATED newAddress1

updatingOperatorScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
updatingOperatorScenario originateFn = do
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
  withSender (AddressResolved owner1) $
    call dao (Call @"Update_operators") [FA2.AddOperator params]
  withSender (AddressResolved owner2) $
    call dao (Call @"Transfer") transferParams

  withSender (AddressResolved owner1) $
    call dao (Call @"Update_operators") [FA2.RemoveOperator params]

  let notOwnerParams = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = owner1
        , opTokenId = unfrozenTokenId
        }

  withSender (AddressResolved owner2) $
    call dao (Call @"Transfer") transferParams
    & expectCustomError_ #fA2_NOT_OPERATOR
  withSender (AddressResolved owner1) $ do
    call dao (Call @"Update_operators") ([FA2.AddOperator notOwnerParams])
      & expectCustomError_ #nOT_OWNER
    call dao (Call @"Update_operators") [FA2.RemoveOperator notOwnerParams]
      & expectCustomError_ #nOT_OWNER

lowBalanceScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
lowBalanceScenario originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 200
            } ]
        } ]
  withSender (AddressResolved op1) $ call dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)

lowBalanceOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
lowBalanceOwnerScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = unfrozenTokenId
            , tdAmount = 200
            } ]
        } ]
  withSender (AddressResolved owner1) $ call dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)

noSourceAccountScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
noSourceAccountScenario originateFn = do
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
  withSender (AddressResolved nonexistent) $ call dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0)

badOperatorScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
badOperatorScenario originateFn = do
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
  withSender (AddressResolved op3) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

emptyTransferListScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
emptyTransferListScenario originateFn = do
  ((_, op1), _, dao, _) <- originateFn
  withSender (AddressResolved op1) $ call dao (Call @"Transfer") []

unknownTokenId :: FA2.TokenId
unknownTokenId = FA2.TokenId 2

validateTokenScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
validateTokenScenario originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith param = withSender (AddressResolved op1) $
        call dao (Call @"Transfer") param

  callWith (params unfrozenTokenId)

  callWith (params frozenTokenId)
    & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
  callWith (params unknownTokenId)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

validateTokenOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
validateTokenOwnerScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith param = withSender (AddressResolved owner1) $
        call dao (Call @"Transfer") param

  callWith (params unfrozenTokenId)

  callWith (params frozenTokenId)
    & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
  callWith (params unknownTokenId)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED


noForeignMoneyScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
noForeignMoneyScenario originateFn = do
  ((owner1, op1), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved op1) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

noForeignMoneyOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
noForeignMoneyOwnerScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved owner1) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

balanceOfOwnerScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
balanceOfOwnerScenario originateFn = do
  ((owner1, _), _, dao, _) <- originateFn
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith param = withSender (AddressResolved owner1) $
      call dao (Call @"Balance_of") param

  callWith (params [ FA2.BalanceRequestItem owner1 unfrozenTokenId ])
  callWith (params [ FA2.BalanceRequestItem owner1 frozenTokenId ])
  callWith (params [])
  callWith (params [ FA2.BalanceRequestItem owner1 unknownTokenId ])
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

adminTransferScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
adminTransferScenario originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params


adminTransferFrozenScenario
  :: forall caps base m param
  . (MonadNettest caps base m, FA2.ParameterC param)
  => OriginateFn param m -> m ()
adminTransferFrozenScenario originateFn = do
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
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params

transferAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, ParameterC param pm)
  => OriginateFn param m -> m ()
transferAfterMigrationScenario originateFn = do
  ((owner1, op1), (owner2, newAddress1), dao, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]

  withSender (AddressResolved admin) $
    call dao (Call @"Migrate") (#newAddress .! newAddress1)
  withSender (AddressResolved newAddress1) $
    call dao (Call @"Confirm_migration") ()
  withSender (AddressResolved op1) $ call dao (Call @"Transfer") params
    & expectMigrated newAddress1

balanceOfRequestAfterMigrationScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, ParameterC param pm)
  => OriginateFn param m -> m ()
balanceOfRequestAfterMigrationScenario originateFn = do
  ((owner1, newAddress1), _, dao, admin) <- originateFn
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith param = withSender (AddressResolved owner1) $
      call dao (Call @"Balance_of") param

  withSender (AddressResolved admin) $
    call dao (Call @"Migrate") (#newAddress .! newAddress1)
  withSender (AddressResolved newAddress1) $
    call dao (Call @"Confirm_migration") ()

  callWith (params [ FA2.BalanceRequestItem owner1 unfrozenTokenId ])
    & expectMigrated newAddress1
