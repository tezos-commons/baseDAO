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
      DaoOriginateData{..} <- originateWithCustomToken
      consumer <- callBalanceOf [] dodDao
      checkStorage (toAddress consumer)
        (toVal [[] :: [FA2.BalanceResponseItem]])

  , nettestScenarioCaps "correctly returns the balance of tokens" $ do
      DaoOriginateData{..} <- originateWithCustomToken
      consumer <- callBalanceOf [(dodOwner1, unfrozenTokens)] dodDao
      checkStorage (toAddress consumer)
        (toVal [[((dodOwner1, unfrozenTokens), 1000 :: Natural)]])

  , nettestScenarioCaps "correctly handles several request items" $ do
      DaoOriginateData{..} <- originateWithCustomToken
      consumer <- callBalanceOf
          [(dodOwner1, frozenTokens), (dodOwner2, unfrozenTokens)] dodDao
      checkStorage (toAddress consumer)
        (toVal [[ ((dodOwner1, frozenTokens), 100 :: Natural)
                , ((dodOwner2, unfrozenTokens), 1000 :: Natural)]])

  , nettestScenarioCaps "fails if requested for an undefined token" $ do
      DaoOriginateData{..} <- originateWithCustomToken
      callBalanceOf [(dodOwner1, frozenTokens), (dodOwner2, unknownTokens)] dodDao
        & expectCustomError_ #fA2_TOKEN_UNDEFINED dodDao
  ]


callBalanceOf
  :: MonadNettest caps base m
  => [(Address, FA2.TokenId)]
  -> TAddress Parameter
  -> m (TAddress [FA2.BalanceResponseItem])
callBalanceOf items dodDao = do
  consumer :: TAddress [FA2.BalanceResponseItem]
      <- originateSimple "consumer" [] contractConsumer
  call dodDao (Call @"Balance_of") $
    FA2.mkFA2View (map (uncurry FA2.BalanceRequestItem) items) consumer
  return consumer
