-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.RegistryDAO.Tests.NotEnoughFrozen
  ( notEnoughFrozen
  ) where

import Prelude

import Test.Tasty (TestTree)

import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

notEnoughFrozen
  :: forall variant. RegistryTestConstraints variant => TestTree
notEnoughFrozen = testScenario "checks it fails if required tokens are not frozen" $ scenario $
  withOriginated @variant
    (\_ s -> s) $
    \(_ ::< wallet1 ::< Nil') _ baseDao _ -> let
      proposalMeta = toProposalMetadata @variant $ TransferProposal 1 [] []
      -- Here we only freeze 2 tokens, but the proposal size and the configuration params
      -- frozen_scale_value, frozen_extra_value set to 1 and 0 means that it requires 6
      -- tokens to be frozen (6 * 1 + 0) because proposal size happen to be 6 here.
      in withSender wallet1 $
         (transfer baseDao $ calling (ep @"Propose") (ProposeParams (toAddress wallet1) 2 proposalMeta))
         & expectFailProposalCheck incorrectTokenAmountErrMsg
