-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.RegistryView
  ( registryView
  ) where

import Prelude

import Test.Tasty (TestTree)

import Lorentz as L hiding (Contract, assert, div)
import Morley.Util.Named
import Test.Cleveland
import Test.Cleveland.Lorentz (contractConsumer)

import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

registryView
  :: forall variant. RegistryTestConstraints variant => TestTree
registryView = testScenario "checks on-chain view correctly returns the registry value" $ scenario $ do
  -- The default values assigned from initialStorageWithExplictRegistryDAOConfig function
  withOriginated @variant 3
    (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $


    \(admin: wallet1: voter1 : _) (toPeriod -> period) baseDao _ -> do
      let
        proposalMeta = toProposalMetadata @variant $ TransferProposal 1 [] [([mt|key|], Just [mt|testVal|])]

        proposalSize = metadataSize proposalMeta

      withSender wallet1 $
        call baseDao (Call @"Freeze") (#amount :! proposalSize)

      withSender voter1 $
        call baseDao (Call @"Freeze") (#amount :! 50)
      -- Advance one voting period to a proposing stage.
      startLevel <- getOriginationLevel' @variant baseDao
      advanceToLevel (startLevel + period)

      let requiredFrozen = proposalSize -- since frozen_scale_value and frozen_scale_value are 1 and 0.

      -- We propose the addition of a new registry key
      withSender wallet1 $
        call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozen proposalMeta)

      -- Advance one voting period to a voting stage.
      advanceToLevel (startLevel + 2*period)
      -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
      let proposalKey = makeProposalKey (ProposeParams wallet1 requiredFrozen proposalMeta)
      withSender voter1 $
        call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 50 voter1) Nothing]

      -- Advance one voting period to a proposing stage.
      proposalStart <- getProposalStartLevel' @variant baseDao proposalKey
      advanceToLevel (proposalStart + 2*period)
      withSender admin $
        call baseDao (Call @"Flush") (1 :: Natural)

      consumer <- chAddress <$> originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))

      withSender voter1 $
        call baseDao (Call @"Lookup_registry") (LookupRegistryParam [mt|key|] consumer)

      checkStorage @[(MText, (Maybe MText))] consumer ([([mt|key|], Just [mt|testVal|])])
