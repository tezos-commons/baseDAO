-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

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
  withOriginated @variant
    (\_ fs ->
      fs &
      setVariantExtra @variant @"FrozenScaleValue" frozen_scale_value &
      setVariantExtra @variant @"FrozenExtraValue" frozen_extra_value &
      setVariantExtra @variant @"SlashScaleValue" slash_scale_value &
      setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) &
      setVariantExtra @variant @"SlashDivisionValue" slash_division_value
      ) $

    \(admin ::< wallet1 ::< voter1 ::< Nil') (toPeriod -> period) baseDao _ -> let

      in do
        withSender wallet1 $
          transfer baseDao $ calling (ep @"Freeze") (#amount :! 400)

        withSender voter1 $
          transfer baseDao $ calling (ep @"Freeze") (#amount :! 100)

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
          transfer baseDao $ calling (ep @"Propose") (ProposeParams (toAddress wallet1) requiredFrozenForUpdate sMaxUpdateproposalMeta1)

        -- Advance one voting period to a voting stage.
        advanceToLevel (startLevel + 2* period)
        -- Then we send 60 upvotes for the proposal (as min quorum is 1% of 500)
        let proposalKey = makeProposalKey (ProposeParams (toAddress wallet1) requiredFrozenForUpdate sMaxUpdateproposalMeta1)
        withSender voter1 $
          transfer baseDao $ calling (ep @"Vote") [PermitProtected (VoteParam proposalKey True 60 (toAddress voter1)) Nothing]

        proposalStart <- getProposalStartLevel' @variant baseDao proposalKey
        advanceToLevel (proposalStart + 2*period)
        withSender admin $
          transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

        maxProposalSize <- ((getVariantExtra @variant @"MaxProposalSize") . sExtraRPC) <$> getVariantStorageRPC @variant (chAddress baseDao)
        assert (maxProposalSize == (341 :: Natural)) "Unexpected max_proposal_size update"
