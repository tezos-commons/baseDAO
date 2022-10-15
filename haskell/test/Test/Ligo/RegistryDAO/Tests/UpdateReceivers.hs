-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.RegistryDAO.Tests.UpdateReceivers
  ( updateReceivers
  ) where

import Prelude

import Data.Set qualified as S
import Test.Tasty (TestTree)

import Morley.Tezos.Address
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
  withOriginated @variant @3
      (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
    \(admin ::< wallet1 ::< wallet2 ::< Nil') (toPeriod -> period) baseDao _ -> do

      let
        proposalMeta = toProposalMetadata @variant $ Add_receivers [MkAddress wallet1, MkAddress wallet2]

        proposalSize = metadataSize proposalMeta
        proposeParams = ProposeParams (MkAddress wallet1) proposalSize proposalMeta

      withSender wallet1 $
        transfer baseDao$ calling (ep @"Freeze") (#amount :! proposalSize)
      withSender wallet2 $
        transfer baseDao$ calling (ep @"Freeze") (#amount :! 20)

      startLevel <- getOriginationLevel' @variant baseDao
      -- Advance one voting period to a proposing stage.
      advanceToLevel (startLevel + period)

      withSender wallet1 $
        transfer baseDao$ calling (ep @"Propose") proposeParams

      checkBalance' @variant baseDao wallet1 proposalSize

      let
        key1 = makeProposalKey proposeParams
        upvote = NoPermit VoteParam
            { vFrom = MkAddress wallet2
            , vVoteType = True
            , vVoteAmount = 20
            , vProposalKey = key1
            }

      -- Advance one voting period to a voting stage.
      advanceToLevel (startLevel + 2*period)
      withSender wallet2 $ transfer baseDao $ calling (ep @"Vote") [upvote]
      -- Advance one voting period to a proposing stage.
      proposalStart <- getProposalStartLevel' @variant baseDao key1
      advanceToLevel (proposalStart + 2*period)
      withSender admin $ transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

      receiversSet <- (getVariantExtra @variant @"ProposalReceivers"  . sExtraRPC) <$> getVariantStorageRPC @variant (chAddress baseDao)
      assert (((MkAddress wallet1) `S.member` receiversSet) && ((MkAddress wallet2) `S.member` receiversSet))
        "Proposal receivers was not updated as expected"
