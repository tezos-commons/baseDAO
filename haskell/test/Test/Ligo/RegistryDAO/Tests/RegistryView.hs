-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

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
  withOriginated @variant @3
    (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $


    \(admin ::< wallet1 ::< voter1 ::< Nil') (toPeriod -> period) baseDao _ -> do
      let
        proposalMeta = toProposalMetadata @variant $ TransferProposal 1 [] [([mt|key|], Just [mt|testVal|])]

        proposalSize = metadataSize proposalMeta

      withSender wallet1 $
        transfer baseDao$ calling (ep @"Freeze") (#amount :! proposalSize)

      withSender voter1 $
        transfer baseDao$ calling (ep @"Freeze") (#amount :! 50)
      -- Advance one voting period to a proposing stage.
      startLevel <- getOriginationLevel' @variant baseDao
      advanceToLevel (startLevel + period)

      let requiredFrozen = proposalSize -- since frozen_scale_value and frozen_scale_value are 1 and 0.

      -- We propose the addition of a new registry key
      withSender wallet1 $
        transfer baseDao$ calling (ep @"Propose") (ProposeParams (toAddress wallet1) requiredFrozen proposalMeta)

      -- Advance one voting period to a voting stage.
      advanceToLevel (startLevel + 2*period)
      -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
      let proposalKey = makeProposalKey (ProposeParams (toAddress wallet1) requiredFrozen proposalMeta)
      withSender voter1 $
        transfer baseDao$ calling (ep @"Vote") [PermitProtected (VoteParam proposalKey True 50 (toAddress voter1)) Nothing]

      -- Advance one voting period to a proposing stage.
      proposalStart <- getProposalStartLevel' @variant baseDao proposalKey
      advanceToLevel (proposalStart + 2*period)
      withSender admin $
        transfer baseDao$ calling (ep @"Flush") (1 :: Natural)

      consumer <- originate "consumer" [] (contractConsumer @(MText, (Maybe MText)))

      withSender voter1 $
        transfer baseDao$ calling (ep @"Lookup_registry") (LookupRegistryParam [mt|key|] (toAddress $ chAddress consumer))

      checkStorage @[(MText, (Maybe MText))] consumer ([([mt|key|], Just [mt|testVal|])])
