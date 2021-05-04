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
          ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved op1) $
            transfer 10 unfrozenTokens owner1 owner2 dao
          -- 1000 initially + 10 received
          assertBalanceOf owner2 1010 unfrozenTokens dao

      , nettestScenarioCaps "aborts if there is a failure (due to low balance)" $ do
          ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved op1) $
            transfer 1001 unfrozenTokens owner1 owner2 dao
              & expectCustomError
                  #fA2_INSUFFICIENT_BALANCE dao
                  (#required .! 1001, #present .! 1000)

      , nettestScenarioCaps "aborts if there is a failure (due to bad operator)" $ do
          op3 :: Address <- newAddress "operator3"
          ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved op1) $
            transfer 10 unfrozenTokens owner2 owner1 dao
              & expectCustomError_ #fA2_NOT_OPERATOR dao

          withSender (AddressResolved op3) $
            transfer 10 unfrozenTokens owner2 owner1 dao
              & expectCustomError_ #fA2_NOT_OPERATOR dao

      , nettestScenarioCaps "cannot transfer frozen tokens" $ do
          ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved op1) $
            transfer 10 frozenTokens owner1 owner2 dao
              & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dao

      , nettestScenarioCaps "fails if trying to transfer unknown tokens" $ do
          ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved op1) $
            transfer 0 unknownTokens owner1 owner2 dao
              & expectCustomError_ #fA2_NOT_OPERATOR dao
      ]

  , testGroup "Transfers by token owners:"
      [ nettestScenarioCaps "accepts an empty list of transfers" $ do
          someone :: Address <- newAddress "someone"
          (_, _, dao, _, _) <- originateWithCustomToken
          withSender (AddressResolved someone) $
            call dao (Call @"Transfer") []

      , nettestScenarioCaps "allows zero transfer from non-existent owner" $ do
          nonexistent :: Address <- newAddress "nonexistent"
          ((owner1, _), _, dao, _,  _) <- originateWithCustomToken
          withSender (AddressResolved nonexistent) $
            transfer 0 unfrozenTokens nonexistent owner1 dao

      , nettestScenarioCaps "allows valid transfer and check balance" $ do
            ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken

            withSender (AddressResolved owner1) $
              transfer 10 unfrozenTokens owner1 owner2 dao
            -- 1000 initially + 10 received
            assertBalanceOf owner2 1010 unfrozenTokens dao

      , nettestScenarioCaps "aborts if there is a failure (due to low balance)" $ do
          ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved owner1) $
            transfer 1001 unfrozenTokens owner1 owner2 dao
              & expectCustomError
                  #fA2_INSUFFICIENT_BALANCE dao
                  (#required .! 1001, #present .! 1000)

      , nettestScenarioCaps "cannot transfer someone else's money" $ do
          ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved owner1) $
            transfer 10 unfrozenTokens owner2 owner1 dao
              & expectCustomError_ #fA2_NOT_OPERATOR dao

      , nettestScenarioCaps "aborts if there is a failure (due to non existent source account)" $ do
          nonexistent :: Address <- newAddress "nonexistent"
          ((owner1, _), _, dao, _, _) <- originateWithCustomToken

          -- Failed with 'fA2_INSUFFICIENT_BALANCE' due to nonexistent account is treated
          -- as an existing account with 0 balance.
          withSender (AddressResolved nonexistent) $
            transfer 1 unfrozenTokens nonexistent owner1 dao
              & expectCustomError #fA2_INSUFFICIENT_BALANCE dao (#required .! 1, #present .! 0)

      , nettestScenarioCaps "cannot transfer frozen tokens" $ do
          ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved owner1) $
            transfer 10 frozenTokens owner1 owner2 dao
              & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dao

      , nettestScenarioCaps "fails if trying to transfer unknown tokens" $ do
          ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken

          withSender (AddressResolved owner1) $
            transfer 0 unknownTokens owner1 owner2 dao
              & expectCustomError_ #fA2_TOKEN_UNDEFINED dao
      ]

  , testGroup "Transfers by the administrator"
      [ nettestScenarioCaps "admin cannot transfer unfrozen tokens that don't belong to them" $ do
          ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken

          withSender (AddressResolved admin) $
            transfer 10 unfrozenTokens owner2 owner1 dao
              & expectCustomError_ #fA2_NOT_OPERATOR dao

      , nettestScenarioCaps "admin cannot transfer frozen tokens from any address to any address" $ do
          ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken

          withSender (AddressResolved admin) $
            transfer 10 frozenTokens owner1 owner2 dao
              & expectCustomErrorNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE dao
      ]
  ]
