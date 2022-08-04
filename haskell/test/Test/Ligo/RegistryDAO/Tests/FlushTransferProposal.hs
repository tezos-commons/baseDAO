-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.FlushTransferProposal
  ( flushTransferProposal
  ) where

import Prelude

import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (Contract, assert, div)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

flushTransferProposal
  :: forall variant. RegistryTestConstraints variant => TestTree
flushTransferProposal = testGroup "TransferProposal Tests"
  [ testScenario "checks it can flush a transfer type proposal (#66)" $ scenario $
      withOriginated @variant 3
        (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
        \[admin, wallet1, wallet2] (toPeriod -> period) baseDao dodTokenContract -> do

          let
            proposalMeta = toProposalMetadata @variant $ TransferProposal
                1
                [ tokenTransferType (toAddress dodTokenContract) wallet2 wallet1]
                []
            proposalSize = metadataSize proposalMeta
            proposeParams = ProposeParams wallet1 proposalSize proposalMeta

          withSender wallet1 $
            call baseDao (Call @"Freeze") (#amount :! proposalSize)
          withSender wallet2 $
            call baseDao (Call @"Freeze") (#amount :! 20)

          -- Advance one voting period to a proposing stage.
          startLevel <- getOriginationLevel' @variant baseDao
          advanceToLevel (startLevel + period)

          withSender wallet1 $
            call baseDao (Call @"Propose") proposeParams

          checkBalance' @variant baseDao wallet1 proposalSize

          let
            key1 = makeProposalKey proposeParams
            upvote = NoPermit VoteParam
                { vFrom = wallet2
                , vVoteType = True
                , vVoteAmount = 20
                , vProposalKey = key1
                }

          -- Advance one voting period to a voting stage.
          advanceToLevel (startLevel + 2*period)
          withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
          -- Advance one voting period to a proposing stage.
          proposalStart <- getProposalStartLevel' @variant baseDao key1
          advanceToLevel (proposalStart + 2*period)
          withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

          checkBalance' @variant baseDao wallet1 proposalSize
          checkBalance' @variant baseDao wallet2 20

          checkStorage (unTAddress dodTokenContract)
            ( [ [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
              , [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 20 }] } ] -- Wallet2 freezes 20 tokens
              , [ FA2.TransferItem { tiFrom = wallet1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
              ])
  , testScenario "checks it can flush a transfer proposal" $ scenario $
        withOriginated @variant 3
            (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) s) $
          \[admin, wallet1, wallet2] (toPeriod -> period) baseDao _ -> do

            let
              proposalMeta = toProposalMetadata @variant $
                  TransferProposal 1 [ xtzTransferType 3 wallet2 ] []
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams wallet1 proposalSize proposalMeta

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount :! proposalSize)
            withSender wallet2 $
              call baseDao (Call @"Freeze") (#amount :! 10)
            sendXtz baseDao
            startLevel <- getOriginationLevel' @variant baseDao

            -- Advance one voting period to a proposing stage.
            advanceToLevel (startLevel + period)

            withSender wallet1 $
              call baseDao (Call @"Propose") proposeParams
            let key1 = makeProposalKey proposeParams

            checkBalance' @variant baseDao wallet1 proposalSize


            let
              upvote = NoPermit VoteParam
                { vFrom = wallet2
                , vVoteType = True
                , vVoteAmount = 2
                , vProposalKey = key1
                }

            -- Advance one voting period to a voting stage.
            advanceToLevel (startLevel + 2*period)
            withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
            proposalStart <- getProposalStartLevel' @variant baseDao key1
            advanceToLevel (proposalStart + 2*period + 1)
            withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

  ]
