-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.UpdateReceivers
  ( updateReceivers
  ) where

import Prelude

import Data.Set qualified as S
import Test.Tasty (TestTree)

import Lorentz as L hiding (Contract, assert, div)
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

updateReceivers
  :: forall variant. RegistryTestConstraints variant => TestTree
updateReceivers = testScenario "checks it can flush an Update_receivers_proposal" $ scenario $
  withOriginated @variant 3
      (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
    \[admin, wallet1, wallet2] (toPeriod -> period) baseDao _ -> do

      let
        proposalMeta = toProposalMetadata @variant $ Add_receivers [wallet1, wallet2]

        proposalSize = metadataSize proposalMeta
        proposeParams = ProposeParams wallet1 proposalSize proposalMeta

      withSender wallet1 $
        call baseDao (Call @"Freeze") (#amount :! proposalSize)
      withSender wallet2 $
        call baseDao (Call @"Freeze") (#amount :! 20)

      startLevel <- getOriginationLevel' @variant baseDao
      -- Advance one voting period to a proposing stage.
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
      -- Advance one voting period to a proposing stage.
      proposalStart <- getProposalStartLevel' @variant baseDao key1
      advanceToLevel (proposalStart + 2*period)
      withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

      receiversSet <- (getVariantExtra @variant @"ProposalReceivers"  . sExtraRPC) <$> getVariantStorageRPC @variant baseDao
      assert ((wallet1 `S.member` receiversSet) && (wallet2 `S.member` receiversSet))
        "Proposal receivers was not updated as expected"
