-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.RegistryDAO.Tests.UpdateGuardian
  ( updateGuardian
  ) where

import Prelude

import Test.Tasty (TestTree)

import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common

updateGuardian
  :: forall variant. RegistryTestConstraints variant => TestTree
updateGuardian = testScenario "checks it can flush a proposal that updates guardian address" $ scenario $
  withOriginated @variant @4
    (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
    \(admin ::< wallet1 ::< wallet2 ::< newGuardian ::< Nil') (toPeriod -> period) baseDao _ -> do

      let
        proposalMeta = toProposalMetadata @variant (toAddress newGuardian)

        proposalSize = metadataSize proposalMeta
        proposeParams = ProposeParams (toAddress wallet1) proposalSize proposalMeta

      withSender wallet1 $
        transfer baseDao $ calling (ep @"Freeze") (#amount :! proposalSize)
      withSender wallet2 $
        transfer baseDao $ calling (ep @"Freeze") (#amount :! 20)

      -- Advance one voting period to a proposing stage.
      startLevel <- getOriginationLevel' @variant baseDao
      advanceToLevel (startLevel + period)

      withSender wallet1 $
        transfer baseDao $ calling (ep @"Propose") proposeParams

      checkBalance' @variant baseDao wallet1 proposalSize

      let
        key1 = makeProposalKey proposeParams
        upvote = NoPermit VoteParam
            { vFrom = toAddress wallet2
            , vVoteType = True
            , vVoteAmount = 20
            , vProposalKey = key1
            }

      -- Advance one voting period to a voting stage.
      advanceToLevel (startLevel + 2*period)
      withSender wallet2 $ transfer baseDao $ calling (ep @"Vote") [upvote]
      proposalStart <- getProposalStartLevel' @variant baseDao key1
      advanceToLevel (proposalStart + 2*period)
      withSender admin $ transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

      checkGuardian' @variant baseDao newGuardian
