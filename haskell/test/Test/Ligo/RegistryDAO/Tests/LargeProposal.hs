-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.LargeProposal
  ( largeProposalTest
  ) where

import Prelude

import Test.Tasty (TestTree)

import Lorentz as L hiding (Contract, assert, div)
import Morley.Michelson.Text (mkMText)
import Morley.Tezos.Address
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

largeProposalTest
  :: forall variant. RegistryTestConstraints variant => TestTree
largeProposalTest =
  testScenario "proposal exceeding max_proposal_size result in error" $ scenario $
    withOriginated @variant 2
      (\_ s -> s) $
      \(_:wallet1:_) _ baseDao _ -> let
        -- In the explicitly set configuration max_proposal_size is set at 100.
        -- And here we create a proposal that is bigger then 100.
        proposalMeta = toProposalMetadata @variant $ TransferProposal 1 [] $
            [(unsafe $ mkMText ("long_key" <> (show @_ @Int t)), Just [mt|long_value|]) | t <- [1..10]]
        proposalSize = metadataSize proposalMeta
        in withSender wallet1 $ (transfer baseDao $ calling
           (ep @"Propose") (ProposeParams (MkAddress wallet1) proposalSize proposalMeta))
            & expectFailProposalCheck tooLargeProposalErrMsg
