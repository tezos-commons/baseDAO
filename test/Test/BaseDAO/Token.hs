-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Token
  ( test_BaseDAO_Token
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
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Token :: TestTree
test_BaseDAO_Token = testGroup "BaseDAO non-FA2 token tests:"
  [ testGroup "Admin:"
    [ nettestScenario "can burn tokens from any accounts" adminBurnScenario
    , nettestScenario "can mint tokens to any accounts" adminMintScenario
    ]
  , testGroup "Other:"
    [ nettestScenario "can call transfer tokens entrypoint" transferContractTokensScenario
    , nettestScenario "can call token address entrypoint" tokenAddressScenario
    ]
  ]

adminBurnScenario :: (Monad m) => NettestImpl m -> m ()
adminBurnScenario = uncapsNettest $ do
  ((owner1, _), _, dao, admin)
    <- originateBaseDaoWithBalance @() () DAO.defaultConfig
          (\o1 _ ->
              [ ((o1, 0), 10)
              , ((o1, 1), 10)
              ]
          )

  callFrom (AddressResolved admin) dao (Call @"Burn") (DAO.BurnParam owner1 DAO.unfrozenTokenId 100)
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 100, #present .! 10)
  callFrom (AddressResolved admin) dao (Call @"Burn") (DAO.BurnParam owner1 DAO.frozenTokenId 100)
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 100, #present .! 10)

  callFrom (AddressResolved admin) dao (Call @"Burn") (DAO.BurnParam owner1 DAO.unfrozenTokenId 10)
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 0
  callFrom (AddressResolved admin) dao (Call @"Burn") (DAO.BurnParam owner1 DAO.frozenTokenId 5)
  checkTokenBalance (DAO.frozenTokenId) dao owner1 5

adminMintScenario :: (Monad m) => NettestImpl m -> m ()
adminMintScenario = uncapsNettest $ do
  ((owner1, _), _, dao, admin)
    <- originateBaseDaoWithBalance @() () DAO.defaultConfig
          (\o1 _ ->
              [ ((o1, 0), 0)
              , ((o1, 1), 0)
              ]
          )

  callFrom (AddressResolved admin) dao (Call @"Mint") (DAO.MintParam owner1 DAO.unfrozenTokenId 100)
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 100
  callFrom (AddressResolved admin) dao (Call @"Mint") (DAO.MintParam owner1 DAO.frozenTokenId 50)
  checkTokenBalance (DAO.frozenTokenId) dao owner1 50

transferContractTokensScenario :: (Monad m) => NettestImpl m -> m ()
transferContractTokensScenario = uncapsNettest $ do
  (_, _, dao, admin) <- originateBaseDao @() ()
  ((owner1, _), (owner2, _), fa2Contract, _) <- originateBaseDao @() ()
  let addParams = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = toAddress dao
        , opTokenId = DAO.unfrozenTokenId
        }
  callFrom (AddressResolved owner1) fa2Contract (Call @"Update_operators") [FA2.AddOperator addParams]

  let transferParams = [ FA2.TransferItem
            { tiFrom = owner1
            , tiTxs = [ FA2.TransferDestination
                { tdTo = owner2
                , tdTokenId = DAO.unfrozenTokenId
                , tdAmount = 10
                } ]
            } ]
      param = DAO.TransferContractTokensParam
        { DAO.tcContractAddress = toAddress fa2Contract
        , DAO.tcParams = transferParams
        }

  callFrom (AddressResolved admin) dao (Call @"Transfer_contract_tokens") param
  checkTokenBalance (DAO.unfrozenTokenId) fa2Contract owner1 90
  checkTokenBalance (DAO.unfrozenTokenId) fa2Contract owner2 110

tokenAddressScenario :: (Monad m) => NettestImpl m -> m ()
tokenAddressScenario = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDao @() ()
  consumer <- originateSimple "consumer" [] contractConsumer
  callFrom (AddressResolved owner1) dao (Call @"Token_address") (toContractRef consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [(toAddress dao)])
