-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token
  ( test_BaseDAO_Token
  ) where

import Universum

import Lorentz hiding ((>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Token :: TestTree
test_BaseDAO_Token = testGroup "BaseDAO non-FA2 token tests:"
  [ nettestScenario "can burn tokens from any accounts"
      $ uncapsNettest $ burnScenario
        $ originateLigoDaoWithBalance dynRecUnsafe defaultConfig
          (\o1 o2 ->
              [ ((o1, unfrozenTokenId), 10)
              , ((o1, frozenTokenId), 10)
              , ((o2, unfrozenTokenId), 10) -- for total supply
              , ((o2, frozenTokenId), 10)-- for total supply
              ]
          )
  , nettestScenario "can mint tokens to any accounts"
      $ uncapsNettest $ mintScenario
        $ originateLigoDaoWithBalance dynRecUnsafe defaultConfig
          (\o1 _ ->
              [ ((o1, unfrozenTokenId), 0)
              , ((o1, frozenTokenId), 0)
              ]
          )
  , nettestScenario "can call transfer tokens entrypoint"
      $ uncapsNettest $ transferContractTokensScenario originateLigoDao
  ]

burnScenario
  :: (MonadNettest caps base m, HasCallStack)
  => OriginateFn m -> m ()
burnScenario originateFn = withFrozenCallStack $ do
  ((owner1, _), _, dao, admin) <- originateFn

  withSender (AddressResolved owner1) $
    call dao (Call @"Burn") (BurnParam owner1 unfrozenTokenId 10)
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $ do
    call dao (Call @"Burn") (BurnParam owner1 unfrozenTokenId 11)
      & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 11, #present .! 10)

    call dao (Call @"Burn") (BurnParam owner1 frozenTokenId 11)
      & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 11, #present .! 10)

    call dao (Call @"Burn") (BurnParam owner1 unfrozenTokenId 10)
  checkTokenBalance (unfrozenTokenId) dao owner1 0
  withSender (AddressResolved admin) $
    call dao (Call @"Burn") (BurnParam owner1 frozenTokenId 5)
  checkTokenBalance (frozenTokenId) dao owner1 5

  -- Check total supply
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
      & expectError (VoidResult (10 :: Natural)) -- initial = 20

  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError (VoidResult (15 :: Natural)) -- initial = 20

mintScenario
  :: (MonadNettest caps base m, HasCallStack)
  => OriginateFn m -> m ()
mintScenario originateFn = withFrozenCallStack $ do
  ((owner1, _), _, dao, admin) <- originateFn

  withSender (AddressResolved owner1) $
    call dao (Call @"Mint") (MintParam owner1 unfrozenTokenId 10)
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $ do
    call dao (Call @"Mint") (MintParam owner1 unfrozenTokenId 100)
  checkTokenBalance (unfrozenTokenId) dao owner1 100
  withSender (AddressResolved admin) $
    call dao (Call @"Mint") (MintParam owner1 frozenTokenId 50)
  checkTokenBalance (frozenTokenId) dao owner1 50

  -- Check total supply
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
      & expectError (VoidResult (100 :: Natural)) -- initial = 0

  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError (VoidResult (50 :: Natural)) -- initial = 0

transferContractTokensScenario
  :: MonadNettest caps base m
  => OriginateFn m -> m ()
transferContractTokensScenario originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn
  ((target_owner1, _), (target_owner2, _), fa2Contract, _) <- originateFn
  let addParams = FA2.OperatorParam
        { opOwner = target_owner1
        , opOperator = toAddress dao
        , opTokenId = unfrozenTokenId
        }
  withSender (AddressResolved target_owner1) $
    call fa2Contract (Call @"Update_operators") [FA2.AddOperator addParams]

  let transferParams = [ FA2.TransferItem
            { tiFrom = target_owner1
            , tiTxs = [ FA2.TransferDestination
                { tdTo = target_owner2
                , tdTokenId = unfrozenTokenId
                , tdAmount = 10
                } ]
            } ]
      param = TransferContractTokensParam
        { tcContractAddress = toAddress fa2Contract
        , tcParams = transferParams
        }

  withSender (AddressResolved owner1) $
    call dao (Call @"Transfer_contract_tokens") param
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $
    call dao (Call @"Transfer_contract_tokens") param
  checkTokenBalance (unfrozenTokenId) fa2Contract target_owner1 90
  checkTokenBalance (unfrozenTokenId) fa2Contract target_owner2 110
