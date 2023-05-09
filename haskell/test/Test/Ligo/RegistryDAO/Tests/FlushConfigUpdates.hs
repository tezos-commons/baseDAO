-- SPDX-FileCopyrightText: 2023 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.RegistryDAO.Tests.FlushConfigUpdates
  ( flushConfigUpdates
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

flushConfigUpdates :: forall variant. RegistryTestConstraints variant => TestTree
flushConfigUpdates =
  testScenario "can correctly flush a config update proposal" $ scenario $ do
    let frozen_scale_value   = 1 :: Natural
        frozen_extra_value   = 0 :: Natural
        slash_scale_value    = 1 :: Natural
        max_proposal_size    = 200 :: Natural
        slash_division_value = 1 :: Natural

        newFrozenScaleValue   = 0 :: Natural
        newFrozenExtraValue   = 1 :: Natural
        newSlashScaleValue    = 10 :: Natural
        newMaxProposalSize    = 500 :: Natural
        newSlashDivisionValue = 0 :: Natural

    withOriginated @variant
      (\_ fs ->
        fs &
        setVariantExtra @variant @"FrozenScaleValue" frozen_scale_value &
        setVariantExtra @variant @"FrozenExtraValue" frozen_extra_value &
        setVariantExtra @variant @"SlashScaleValue" slash_scale_value &
        setVariantExtra @variant @"MaxProposalSize" max_proposal_size &
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
                    { cpFrozenScaleValue = Just newFrozenScaleValue
                    , cpFrozenExtraValue = Just newFrozenExtraValue
                    , cpSlashScaleValue = Just newSlashScaleValue
                    , cpMaxProposalSize = Just newMaxProposalSize
                    , cpSlashDivisionValue = Just newSlashDivisionValue
                    }
              sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
              requiredFrozenForUpdate = sMaxUpdateproposalSize1 * frozen_scale_value + frozen_extra_value

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

          variantExtraRPC <- sExtraRPC <$> getVariantStorageRPC @variant (chAddress baseDao)
          let frozenScaleValue   = getVariantExtra @variant @"FrozenScaleValue" variantExtraRPC
              frozenExtraValue   = getVariantExtra @variant @"FrozenExtraValue" variantExtraRPC
              slashScaleValue    = getVariantExtra @variant @"SlashScaleValue" variantExtraRPC
              maxProposalSize    = getVariantExtra @variant @"MaxProposalSize" variantExtraRPC
              slashDivisionValue = getVariantExtra @variant @"SlashDivisionValue" variantExtraRPC

          assert (frozenScaleValue == newFrozenScaleValue) $
            "Unexpected frozen_scale_value update: " <> show frozenScaleValue
          assert (frozenExtraValue == newFrozenExtraValue) $
            "Unexpected frozen_extra_value update: " <> show frozenExtraValue
          assert (slashScaleValue == newSlashScaleValue) $
            "Unexpected slash_scale_value update: " <> show slashScaleValue
          assert (maxProposalSize == newMaxProposalSize) $
            "Unexpected max_proposal_size update: " <> show maxProposalSize
          assert (slashDivisionValue == newSlashDivisionValue) $
            "Unexpected slash_division_value update: " <> show slashDivisionValue
