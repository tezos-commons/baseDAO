-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)

import Test.Ligo.BaseDAO.Token.Transfer (transferTests)
import Test.Ligo.BaseDAO.Token.BalanceOf (balanceOfTests)
import Test.Ligo.BaseDAO.Token.UpdateOperators (updateOperatorsTests)

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:" $
  [ transferTests
  , balanceOfTests
  , updateOperatorsTests
  ]
