-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.RequiredFrozen
  ( requiredFrozen
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

requiredFrozen
  :: forall variant. RegistryTestConstraints variant => TestTree
requiredFrozen = testScenario "check it correctly calculates required frozen tokens" $ scenario $
  withOriginated @variant 2
    (\_ s -> setVariantExtra @variant @"FrozenExtraValue" (2 :: Natural) s) $
    \(_:wallet1:_) fs baseDao _ -> do
      let
        proposalMeta = toProposalMetadata @variant $ TransferProposal 1 [] []
        proposalSize = metadataSize proposalMeta -- 10

      withSender wallet1 $
        call baseDao (Call @"Freeze") (#amount :! (proposalSize + 2))

      startLevel <- getOriginationLevel' @variant baseDao

      -- Advance one voting period to a proposing stage.
      advanceToLevel (startLevel + toPeriod fs)

      -- Here the proposal size and the configuration params frozen_scale_value,
      -- frozen_extra_value set to 1 and 2 means that it requires 12 tokens to be
      -- frozen (10 * 1 + 2) if proposal size is 10.
      withSender wallet1 $
         call baseDao (Call @"Propose") (ProposeParams wallet1 (proposalSize + 2) proposalMeta)
