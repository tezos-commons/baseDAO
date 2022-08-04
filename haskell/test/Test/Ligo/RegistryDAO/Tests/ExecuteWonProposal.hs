-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.ExecuteWonProposal
  ( executeWonProposal
  ) where

import Prelude

import Test.Tasty (TestTree)

import Lorentz as L hiding (Contract, assert, div)
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

executeWonProposal
  :: forall variant. RegistryTestConstraints variant => TestTree
executeWonProposal = testScenario "checks it correctly executes the proposal that has won" $ scenario $ do
  let frozen_scale_value = 1 :: Natural
  let frozen_extra_value = 0 :: Natural
  let slash_scale_value = 1 :: Natural
  let slash_division_value = 1 :: Natural
  withOriginated @variant 3
    (\_ fs ->
      fs &
      setVariantExtra @variant @"FrozenScaleValue" frozen_scale_value &
      setVariantExtra @variant @"FrozenExtraValue" frozen_extra_value &
      setVariantExtra @variant @"SlashScaleValue" slash_scale_value &
      setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) &
      setVariantExtra @variant @"SlashDivisionValue" slash_division_value
      ) $

    \(admin: wallet1: voter1 : _) (toPeriod -> period) baseDao _ -> let

      in do
        withSender wallet1 $
          call baseDao (Call @"Freeze") (#amount :! 400)

        withSender voter1 $
          call baseDao (Call @"Freeze") (#amount :! 100)

        startLevel <- getOriginationLevel' @variant baseDao
        -- Advance one voting period to a proposing stage.
        advanceToLevel (startLevel + period)

        let sMaxUpdateproposalMeta1 = toProposalMetadata @variant $ ConfigProposal
                  { cpFrozenScaleValue = Nothing
                  , cpFrozenExtraValue = Nothing
                  , cpSlashScaleValue = Nothing
                  , cpSlashDivisionValue = Nothing
                  , cpMaxProposalSize = Just 341
                  }
        let sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
        let requiredFrozenForUpdate = sMaxUpdateproposalSize1 * frozen_scale_value + frozen_extra_value

        withSender wallet1 $
          call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozenForUpdate sMaxUpdateproposalMeta1)

        -- Advance one voting period to a voting stage.
        advanceToLevel (startLevel + 2* period)
        -- Then we send 60 upvotes for the proposal (as min quorum is 1% of 500)
        let proposalKey = makeProposalKey (ProposeParams wallet1 requiredFrozenForUpdate sMaxUpdateproposalMeta1)
        withSender voter1 $
          call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 60 voter1) Nothing]

        proposalStart <- getProposalStartLevel' @variant baseDao proposalKey
        advanceToLevel (proposalStart + 2*period)
        withSender admin $
          call baseDao (Call @"Flush") (1 :: Natural)

        maxProposalSize <- ((getVariantExtra @variant @"MaxProposalSize") . sExtraRPC) <$> getVariantStorageRPC @variant baseDao
        assert (maxProposalSize == (341 :: Natural)) "Unexpected max_proposal_size update"

