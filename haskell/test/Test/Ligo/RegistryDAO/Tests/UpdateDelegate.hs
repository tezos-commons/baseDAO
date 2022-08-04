-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.UpdateDelegate
  ( updateDelegate
  ) where

import Prelude

import Test.Tasty (TestTree)

import Lorentz as L hiding (Contract, assert, div)
import Morley.Tezos.Address
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common

updateDelegate
  :: forall variant. RegistryTestConstraints variant => TestTree
updateDelegate = testScenario "checks it can flush a proposal that updates contract delegate address" $ scenario $
  withOriginated @variant 4
      (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
    \[admin, wallet1, wallet2, delegateAddr] (toPeriod -> period) baseDao _ -> do
      registerDelegate delegateAddr
      case delegateAddr of
        KeyAddress delegate -> do
          let
            proposalMeta = toProposalMetadata @variant $ Just delegate

            proposalSize = metadataSize proposalMeta
            proposeParams = ProposeParams wallet1 proposalSize proposalMeta

          withSender wallet1 $
            call baseDao (Call @"Freeze") (#amount :! proposalSize)
          withSender wallet2 $
            call baseDao (Call @"Freeze") (#amount :! 20)

          -- Advance one voting period to a proposing stage.
          startLevel <- getOriginationLevel' @variant baseDao
          advanceToLevel (startLevel + period)

          withSender wallet1 $
            call baseDao (Call @"Propose") proposeParams

          checkBalance' @variant baseDao wallet1 proposalSize

          let
            key1 = makeProposalKey proposeParams
            upvote = NoPermit VoteParam
                { vFrom = wallet2
                , vVoteType = True
                , vVoteAmount = 20
                , vProposalKey = key1
                }

          -- Advance one voting period to a voting stage.
          advanceToLevel (startLevel + 2*period)
          withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
          proposalStart <- getProposalStartLevel' @variant baseDao key1
          advanceToLevel (proposalStart + 2*period)
          withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
          getDelegate baseDao @@== (Just delegate)
        _ -> error "impossible"
