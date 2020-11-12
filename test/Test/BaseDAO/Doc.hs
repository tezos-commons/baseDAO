-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Doc
  ( test_baseDAO_documentation
  , test_gameDAO_documentation
  ) where

import Prelude
import Test.Tasty (TestTree, testGroup)

import Lorentz (buildLorentzDoc, cCode)
import Lorentz.Test
import Lorentz.Contracts.BaseDAO (baseDaoContract, defaultConfig)
import Lorentz.Contracts.GameDAO (gameDaoContract)

test_baseDAO_documentation :: TestTree
test_baseDAO_documentation = testGroup "baseDAO documentation" $
  runDocTests testLorentzDoc $ buildLorentzDoc $ cCode $ baseDaoContract (defaultConfig @())

test_gameDAO_documentation :: TestTree
test_gameDAO_documentation = testGroup "gameDAO documentation" $
  runDocTests testLorentzDoc $ buildLorentzDoc $ cCode gameDaoContract
