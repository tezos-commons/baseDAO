-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains FA2 tests for testing the Ligo contract.
module Test.Ligo.BaseDAO.FA2
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
  , prohibitedAdminTransferScenario
  , adminTransferFrozenScenario
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test hiding (withSender)
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

unfrozenTokenId :: FA2.TokenId
unfrozenTokenId = FA2.TokenId 42

unknownTokenId :: FA2.TokenId
unknownTokenId = FA2.TokenId 2

originateWithCustomToken :: MonadNettest caps base m => OriginateFn m
originateWithCustomToken =
  originateLigoDaoWithBalance dynRecUnsafe defaultConfig
      (\owner1 owner2 ->
          [ ((owner1, frozenTokenId), 100)
          , ((owner2, frozenTokenId), 100)
          , ((owner1, FA2.TokenId 42), 1000)
          , ((owner2, FA2.TokenId 42), 1000)
          ]
      )

zeroTransferScenario :: MonadNettest caps base m => OriginateFn m -> m ()
zeroTransferScenario originateFn = do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _,  _) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = nonexistent
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = frozenTokenId
            , tdAmount = 0
            } ]
        } ]

  withSender (AddressResolved nonexistent) $
    call dao (Call @"Transfer") params

validTransferScenario :: MonadNettest caps base m => OriginateFn m -> m ()
validTransferScenario originateFn = do
  ((owner1, _op1), (owner2, _), dao, _, admin) <- originateFn
  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = frozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved admin) $ call
    dao (Call @"Transfer") params

  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  withSender (AddressResolved admin) $ call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = owner2
      , briTokenId = frozenTokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, frozenTokenId), 110 :: Natural)]] )

validTransferOwnerScenario :: MonadNettest caps base m => m ()
validTransferOwnerScenario = do
   ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken
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
     (toVal [[((owner2, unfrozenTokenId), 1010 :: Natural)]] )

updatingOperatorScenario :: MonadNettest caps base m => m ()
updatingOperatorScenario = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken
  let params tokenId = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = admin
        , opTokenId = tokenId
        }
      transferParams tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved owner1) $
    call dao (Call @"Update_operators") [FA2.AddOperator (params frozenTokenId)]
  withSender (AddressResolved admin) $
    call dao (Call @"Transfer") (transferParams frozenTokenId)

  withSender (AddressResolved owner1) $
    call dao (Call @"Update_operators") [FA2.RemoveOperator (params frozenTokenId)]

  let notOwnerParams tokenId = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = admin
        , opTokenId = tokenId
        }

  withSender (AddressResolved owner2) $
    call dao (Call @"Transfer") (transferParams frozenTokenId)
    & expectCustomError_ #fA2_NOT_OPERATOR dao
  withSender (AddressResolved owner1) $ do
    call dao (Call @"Update_operators") ([FA2.AddOperator (notOwnerParams frozenTokenId)])
      & expectCustomErrorNoArg #nOT_OWNER dao
    call dao (Call @"Update_operators") [FA2.RemoveOperator (notOwnerParams frozenTokenId)]
      & expectCustomErrorNoArg #nOT_OWNER dao

lowBalanceScenario :: MonadNettest caps base m => OriginateFn m -> m ()
lowBalanceScenario originateFn = do
  ((owner1, _op1), (owner2, _), dao, _, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = frozenTokenId
            , tdAmount = 200
            } ]
        } ]
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE dao (#required .! 200, #present .! 100)

lowBalanceOwnerScenario :: MonadNettest caps base m => OriginateFn m -> m ()
lowBalanceOwnerScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = frozenTokenId
            , tdAmount = 200
            } ]
        } ]
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE dao (#required .! 200, #present .! 100)

noSourceAccountScenario :: MonadNettest caps base m => OriginateFn m -> m ()
noSourceAccountScenario originateFn = do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = nonexistent
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = frozenTokenId
            , tdAmount = 1
            } ]
        } ]

  -- Failed with 'fA2_INSUFFICIENT_BALANCE' due to nonexistent account is treated
  -- as an existing account with 0 balance.
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE dao (#required .! 1, #present .! 0)

badOperatorScenario :: MonadNettest caps base m => OriginateFn m -> m ()
badOperatorScenario originateFn = do
  op3  :: Address <- newAddress "operator3"
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = frozenTokenId
            , tdAmount = 0
            } ]
        } ]
  withSender (AddressResolved op3) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR dao

emptyTransferListScenario :: MonadNettest caps base m => OriginateFn m -> m ()
emptyTransferListScenario originateFn = do
  ((_, op1), _, dao, _, _) <- originateFn
  withSender (AddressResolved op1) $ call dao (Call @"Transfer") []

validateTokenScenario :: MonadNettest caps base m => m ()
validateTokenScenario = do
  ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

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
    & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dao
  callWith (params unknownTokenId)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED dao


validateTokenOwnerScenario :: MonadNettest caps base m => m ()
validateTokenOwnerScenario = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken

  let params tokenId = [ FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner2
            , tdTokenId = tokenId
            , tdAmount = 10
            } ]
        } ]
      callWith param _sender = withSender (AddressResolved _sender) $
        call dao (Call @"Transfer") param

  callWith (params frozenTokenId) admin

  callWith (params frozenTokenId) owner1
    & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dao
  callWith (params unknownTokenId) owner1
    & expectCustomError_ #fA2_TOKEN_UNDEFINED dao
  callWith (params unfrozenTokenId) admin
    & expectCustomError_ #fA2_NOT_OPERATOR dao

noForeignMoneyScenario :: MonadNettest caps base m => m ()
noForeignMoneyScenario = do
  ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved op1) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR dao

noForeignMoneyOwnerScenario :: MonadNettest caps base m => m ()
noForeignMoneyOwnerScenario = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved owner1) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR dao

balanceOfOwnerScenario :: MonadNettest caps base m => OriginateFn m -> m ()
balanceOfOwnerScenario originateFn = do
  ((owner1, _), _, dao, _, _) <- originateFn
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith param = withSender (AddressResolved owner1) $
      call dao (Call @"Balance_of") param

  callWith (params [ FA2.BalanceRequestItem owner1 frozenTokenId ])
  callWith (params [])
  callWith (params [ FA2.BalanceRequestItem owner1 unknownTokenId ])
    & expectCustomError_ #fA2_TOKEN_UNDEFINED dao

adminTransferScenario :: MonadNettest caps base m => OriginateFn m -> m ()
adminTransferScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateFn

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = frozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params

prohibitedAdminTransferScenario :: MonadNettest caps base m => m ()
prohibitedAdminTransferScenario = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken

  let params = [ FA2.TransferItem
        { tiFrom = owner2
        , tiTxs = [ FA2.TransferDestination
            { tdTo = owner1
            , tdTokenId = unfrozenTokenId
            , tdAmount = 10
            } ]
        } ]
  withSender (AddressResolved admin) $ call dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR dao

adminTransferFrozenScenario :: MonadNettest caps base m => OriginateFn m -> m ()
adminTransferFrozenScenario originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin)
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

