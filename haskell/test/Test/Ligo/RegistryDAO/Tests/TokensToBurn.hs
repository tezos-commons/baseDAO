-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.RegistryDAO.Tests.TokensToBurn
  ( tokensToBurn
  ) where

import Prelude

import Test.Tasty (TestTree)

import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

tokensToBurn
  :: forall variant. RegistryTestConstraints variant => TestTree
tokensToBurn = testScenario "checks it correctly calculates tokens to burn when rejecting" $ scenario $ do
  let frozen_scale_value = 2
  let frozen_extra_value = 0
  let slash_scale_value = 1
  let slash_division_value = 4
  withOriginated @variant @2
    (\_ fs -> fs &
      setVariantExtra @variant @"FrozenScaleValue" frozen_scale_value & -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
      setVariantExtra @variant @"FrozenExtraValue" frozen_extra_value & -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
      setVariantExtra @variant @"SlashScaleValue" slash_scale_value & -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
      setVariantExtra @variant @"SlashDivisionValue" slash_division_value -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
      ) $

    \(admin ::< wallet1 ::< Nil') (toPeriod -> period) baseDao _ -> let
      proposalMeta1 = toProposalMetadata @variant $ TransferProposal 1 [] []
      proposalSize1 = metadataSize proposalMeta1

      in do
        let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

        withSender wallet1 $
          transfer baseDao $ calling (ep @"Freeze") (#amount :! requiredFrozen)

        startLevel <- getOriginationLevel' @variant baseDao

        -- Advance one voting period to a proposing stage.
        advanceToLevel (startLevel + period)

        let params = ProposeParams (toAddress wallet1) requiredFrozen proposalMeta1
        let proposalKey = makeProposalKey params
        withSender wallet1 $
          transfer baseDao $ calling (ep @"Propose") params

        -- Advance one voting period to a proposing stage.
        proposalStart <- getProposalStartLevel' @variant baseDao proposalKey
        advanceToLevel (proposalStart + 2*period + 1)
        withSender admin $
          transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

        -- Since we have frozen_scale_value = 2, slash_scale_value = 1 and slash_division_value = 2
        -- After the rejection above, we expect that (2 * proposalSize1/2) tokens
        -- will be returned to the proposer.  So they will have a balance
        -- of 100 - proposalSize1.

        let spent = div (requiredFrozen * slash_scale_value) slash_division_value


        checkBalance' @variant baseDao wallet1 (requiredFrozen - spent)
