-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BaseDAO.Doc
  ( test_baseDAO_documentation
  , test_gameDAO_documentation
  ) where

import Prelude
import Test.Tasty (TestTree, testGroup)

import Lorentz (cCode)
import Lorentz.Contracts.GameDAO (gameDaoContract)
import Lorentz.Contracts.TrivialDAO (trivialDaoContract)
import Lorentz.Test

test_baseDAO_documentation :: TestTree
test_baseDAO_documentation = testGroup "trivial baseDAO documentation" $
  runDocTests testLorentzDoc $ cCode trivialDaoContract

test_gameDAO_documentation :: TestTree
test_gameDAO_documentation = testGroup "gameDAO documentation" $
  runDocTests testLorentzDoc $ cCode gameDaoContract
