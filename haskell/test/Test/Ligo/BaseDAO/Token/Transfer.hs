-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token.Transfer
  ( transferTests
  ) where

import Universum

import Lorentz (Address)
import Morley.Nettest hiding (transfer)
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.Tasty (TestTree, testGroup)
import Util.Named ((.!))

import Test.Ligo.BaseDAO.Token.Common


transferTests :: TestTree
transferTests = testGroup "Transfer:"
  [ testGroup "Transfers by operators:"
      [ nettestScenarioCaps "allows valid transfer and check balance" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOperator1) $
            transfer 10 unfrozenTokens dodOwner1 dodOwner2 dodDao
          -- 1000 initially + 10 received
          assertBalanceOf dodOwner2 1010 unfrozenTokens dodDao

      , nettestScenarioCaps "aborts if there is a failure (due to low balance)" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOperator1) $
            transfer 1001 unfrozenTokens dodOwner1 dodOwner2 dodDao
              & expectCustomError
                  #fA2_INSUFFICIENT_BALANCE dodDao
                  (#required .! 1001, #present .! 1000)

      , nettestScenarioCaps "aborts if there is a failure (due to bad operator)" $ do
          op3 :: Address <- newAddress "operator3"
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOperator1) $
            transfer 10 unfrozenTokens dodOwner2 dodOwner1 dodDao
              & expectCustomError_ #fA2_NOT_OPERATOR dodDao

          withSender (AddressResolved op3) $
            transfer 10 unfrozenTokens dodOwner2 dodOwner1 dodDao
              & expectCustomError_ #fA2_NOT_OPERATOR dodDao

      , nettestScenarioCaps "cannot transfer frozen tokens" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOperator1) $
            transfer 10 frozenTokens dodOwner1 dodOwner2 dodDao
              & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dodDao

      , nettestScenarioCaps "fails if trying to transfer unknown tokens" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOperator1) $
            transfer 0 unknownTokens dodOwner1 dodOwner2 dodDao
              & expectCustomError_ #fA2_NOT_OPERATOR dodDao
      ]

  , testGroup "Transfers by token owners:"
      [ nettestScenarioCaps "accepts an empty list of transfers" $ do
          someone :: Address <- newAddress "someone"
          DaoOriginateData{..} <- originateWithCustomToken
          withSender (AddressResolved someone) $
            call dodDao (Call @"Transfer") []

      , nettestScenarioCaps "allows zero transfer from non-existent owner" $ do
          nonexistent :: Address <- newAddress "nonexistent"
          DaoOriginateData{..} <- originateWithCustomToken
          withSender (AddressResolved nonexistent) $
            transfer 0 unfrozenTokens nonexistent dodOwner1 dodDao

      , nettestScenarioCaps "allows valid transfer and check balance" $ do
            DaoOriginateData{..} <- originateWithCustomToken

            withSender (AddressResolved dodOwner1) $
              transfer 10 unfrozenTokens dodOwner1 dodOwner2 dodDao
            -- 1000 initially + 10 received
            assertBalanceOf dodOwner2 1010 unfrozenTokens dodDao

      , nettestScenarioCaps "aborts if there is a failure (due to low balance)" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOwner1) $
            transfer 1001 unfrozenTokens dodOwner1 dodOwner2 dodDao
              & expectCustomError
                  #fA2_INSUFFICIENT_BALANCE dodDao
                  (#required .! 1001, #present .! 1000)

      , nettestScenarioCaps "cannot transfer someone else's money" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOwner1) $
            transfer 10 unfrozenTokens dodOwner2 dodOwner1 dodDao
              & expectCustomError_ #fA2_NOT_OPERATOR dodDao

      , nettestScenarioCaps "aborts if there is a failure (due to non existent source account)" $ do
          nonexistent :: Address <- newAddress "nonexistent"
          DaoOriginateData{..} <- originateWithCustomToken

          -- Failed with 'fA2_INSUFFICIENT_BALANCE' due to nonexistent account is treated
          -- as an existing account with 0 balance.
          withSender (AddressResolved nonexistent) $
            transfer 1 unfrozenTokens nonexistent dodOwner1 dodDao
              & expectCustomError #fA2_INSUFFICIENT_BALANCE dodDao (#required .! 1, #present .! 0)

      , nettestScenarioCaps "cannot transfer frozen tokens" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOwner1) $
            transfer 10 frozenTokens dodOwner1 dodOwner2 dodDao
              & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dodDao

      , nettestScenarioCaps "fails if trying to transfer unknown tokens" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodOwner1) $
            transfer 0 unknownTokens dodOwner1 dodOwner2 dodDao
              & expectCustomError_ #fA2_TOKEN_UNDEFINED dodDao
      ]

  , testGroup "Transfers by the administrator"
      [ nettestScenarioCaps "admin cannot transfer unfrozen tokens that don't belong to them" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodAdmin) $
            transfer 10 unfrozenTokens dodOwner2 dodOwner1 dodDao
              & expectCustomError_ #fA2_NOT_OPERATOR dodDao

      , nettestScenarioCaps "admin cannot transfer frozen tokens from any address to any address" $ do
          DaoOriginateData{..} <- originateWithCustomToken

          withSender (AddressResolved dodAdmin) $
            transfer 10 frozenTokens dodOwner1 dodOwner2 dodDao
              & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dodDao
      ]
  ]
