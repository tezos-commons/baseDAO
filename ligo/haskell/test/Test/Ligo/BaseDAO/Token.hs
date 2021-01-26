-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token
  ( test_BaseDAO_Token
  ) where

import Universum

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)

import qualified BaseDAO.ShareTest.Token as Share
import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Token :: TestTree
test_BaseDAO_Token = testGroup "BaseDAO non-FA2 token tests:"
  [ nettestScenario "can burn tokens from any accounts"
      $ uncapsNettest $ Share.burnScenario
        $ originateLigoDaoWithBalance dynRecUnsafe defaultConfigL
          (\o1 _ ->
              [ ((o1, DAO.unfrozenTokenId), 10)
              , ((o1, DAO.frozenTokenId), 10)
              ]
          )
  , nettestScenario "can mint tokens to any accounts"
      $ uncapsNettest $ Share.mintScenario
        $ originateLigoDaoWithBalance dynRecUnsafe defaultConfigL
          (\o1 _ ->
              [ ((o1, DAO.unfrozenTokenId), 0)
              , ((o1, DAO.frozenTokenId), 0)
              ]
          )
  , nettestScenario "can call transfer tokens entrypoint"
      $ uncapsNettest $ Share.transferContractTokensScenario originateLigoDao
  ]
