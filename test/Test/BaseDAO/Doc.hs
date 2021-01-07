-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BaseDAO.Doc
  ( test_baseDAO_documentation
  , test_gameDAO_documentation
  ) where

import Prelude
import Test.Tasty (TestTree, testGroup)

import Lorentz ((:->), iAnyCode, cCode)
import Lorentz.Contracts.GameDAO (gameDaoContract)
import Lorentz.Contracts.TrivialDAO (trivialDaoContract)
import Lorentz.Test
import Michelson.Doc (ContainsDoc(..))

-- TODO: remove by updating to lorentz-0.9
-- Note: this wasn't done right away because such lorentz version does not
-- compile with the latest indigo version available.
instance ContainsDoc (i :-> o) where
  buildDocUnfinalized = buildDocUnfinalized . iAnyCode

test_baseDAO_documentation :: TestTree
test_baseDAO_documentation = testGroup "trivial baseDAO documentation" $
  runDocTests testLorentzDoc $ cCode trivialDaoContract

test_gameDAO_documentation :: TestTree
test_gameDAO_documentation = testGroup "gameDAO documentation" $
  runDocTests testLorentzDoc $ cCode gameDaoContract

