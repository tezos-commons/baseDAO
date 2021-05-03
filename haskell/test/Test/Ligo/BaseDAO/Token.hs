-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token
  ( test_BaseDAO_Token
  ) where

import Universum

import Lorentz hiding ((>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
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
              [ ((o1, frozenTokenId), 10)
              , ((o2, frozenTokenId), 10)-- for total supply
              ]
          )
  , nettestScenario "can call transfer tokens entrypoint"
      $ uncapsNettest $ transferContractTokensScenario originateLigoDao
  ]

burnScenario
  :: (MonadNettest caps base m, HasCallStack)
  => OriginateFn m -> m ()
burnScenario originateFn = withFrozenCallStack $ do
  ((owner1, _), _, dao, _, admin) <- originateFn

  withSender (AddressResolved owner1) $
    call dao (Call @"Burn") (BurnParam owner1 frozenTokenId 10)
    & expectCustomErrorNoArg #nOT_ADMIN dao

  withSender (AddressResolved admin) $ do
    call dao (Call @"Burn") (BurnParam owner1 frozenTokenId 11)
      & expectCustomError #fA2_INSUFFICIENT_BALANCE dao (#required .! 11, #present .! 10)

    call dao (Call @"Burn") (BurnParam owner1 frozenTokenId 11)
      & expectCustomError #fA2_INSUFFICIENT_BALANCE dao (#required .! 11, #present .! 10)

  withSender (AddressResolved admin) $ do
    call dao (Call @"Burn") (BurnParam owner1 frozenTokenId 5)
  checkTokenBalance frozenTokenId dao owner1 5

  -- Check total supply
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError dao (VoidResult (15 :: Natural)) -- initial = 20

transferContractTokensScenario
  :: MonadNettest caps base m
  => OriginateFn m -> m ()
transferContractTokensScenario originateFn = do
  ((owner1, _), _, dao, fa2Contract, admin) <- originateFn
  let target_owner1 = genesisAddress1
  let target_owner2 = genesisAddress2

  let transferParams = [ FA2.TransferItem
            { tiFrom = target_owner1
            , tiTxs = [ FA2.TransferDestination
                { tdTo = target_owner2
                , tdTokenId = FA2.theTokenId
                , tdAmount = 10
                } ]
            } ]
      param = TransferContractTokensParam
        { tcContractAddress = toAddress fa2Contract
        , tcParams = transferParams
        }

  withSender (AddressResolved owner1) $
    call dao (Call @"Transfer_contract_tokens") param
    & expectCustomErrorNoArg #nOT_ADMIN dao

  withSender (AddressResolved admin) $
    call dao (Call @"Transfer_contract_tokens") param

  checkStorage (AddressResolved $ unTAddress fa2Contract)
    (toVal
      [ [ FA2.TransferItem { tiFrom = target_owner1, tiTxs = [FA2.TransferDestination { tdTo = target_owner2, tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ]
      ])
