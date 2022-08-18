-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.ProposeXtzProposal
  ( proposeXtzProposal
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

proposeXtzProposal
  :: forall variant. RegistryTestConstraints variant => TestTree
proposeXtzProposal = testScenario "checks it can propose a valid xtz type proposal (#66)" $ scenario $
  withOriginated @variant 2
    (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
    \[_, wallet] (toPeriod -> period) baseDao _ -> do
      let
        proposalMeta amt = toProposalMetadata @variant $
            TransferProposal 1 [ xtzTransferType amt wallet ] []
        proposeParams amt = ProposeParams wallet (metadataSize $ proposalMeta amt) $ proposalMeta amt
        proposalSize amt = metadataSize $ proposalMeta amt

      withSender wallet $
        call baseDao (Call @"Freeze") (#amount :! proposalSize 10)

      startLevel <- getOriginationLevel' @variant baseDao
      -- Advance one voting period to a proposing stage.
      advanceToLevel (startLevel + period)

      -- Fails because 10 >= max_xtz_amount
      withSender wallet $
        call baseDao (Call @"Propose") (proposeParams 10)
        & expectFailProposalCheck tooLargeXtzErrMsg

      -- Fails because 1 >= min_xtz_amount
      withSender wallet $
        call baseDao (Call @"Propose") (proposeParams 1)
        & expectFailProposalCheck tooSmallXtzErrMsg

      withSender wallet $
        call baseDao (Call @"Freeze") (#amount :! proposalSize 3)

      -- Advance two voting period to another proposing stage.
      advanceToLevel (startLevel + 3*period)

      withSender wallet $
        call baseDao (Call @"Propose") (proposeParams 3)

      checkBalance' @variant baseDao wallet (proposalSize 3 + proposalSize 10)
