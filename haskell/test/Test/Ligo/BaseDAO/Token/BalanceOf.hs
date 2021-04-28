-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token.BalanceOf
  ( balanceOfTests
  ) where

import Universum

import Lorentz (Address, TAddress, toAddress, toVal)
import Lorentz.Test (contractConsumer)
import Morley.Nettest hiding (transfer)
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Types (Parameter)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Token.Common

balanceOfTests :: TestTree
balanceOfTests = testGroup "Balance_of:" $
  [ nettestScenarioCaps "returns an empty response on empty request" $ do
      (_, _, dao, _, _) <- originateWithCustomToken
      consumer <- callBalanceOf [] dao
      checkStorage (toAddress consumer)
        (toVal [[] :: [FA2.BalanceResponseItem]])

  , nettestScenarioCaps "correctly returns the balance of tokens" $ do
      ((owner1, _), _, dao, _, _) <- originateWithCustomToken
      consumer <- callBalanceOf [(owner1, unfrozenTokens)] dao
      checkStorage (toAddress consumer)
        (toVal [[((owner1, unfrozenTokens), 1000 :: Natural)]])

  , nettestScenarioCaps "correctly handles several request items" $ do
      ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken
      consumer <- callBalanceOf
          [(owner1, frozenTokens), (owner2, unfrozenTokens)] dao
      checkStorage (toAddress consumer)
        (toVal [[ ((owner1, frozenTokens), 100 :: Natural)
                , ((owner2, unfrozenTokens), 1000 :: Natural)]])

  , nettestScenarioCaps "fails if requested for an undefined token" $ do
      ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken
      callBalanceOf [(owner1, frozenTokens), (owner2, unknownTokens)] dao
        & expectCustomError_ #fA2_TOKEN_UNDEFINED dao
  ]


callBalanceOf
  :: MonadNettest caps base m
  => [(Address, FA2.TokenId)]
  -> TAddress Parameter
  -> m (TAddress [FA2.BalanceResponseItem])
callBalanceOf items dao = do
  consumer :: TAddress [FA2.BalanceResponseItem]
      <- originateSimple "consumer" [] contractConsumer
  call dao (Call @"Balance_of") $
    FA2.mkFA2View (map (uncurry FA2.BalanceRequestItem) items) consumer
  return consumer
