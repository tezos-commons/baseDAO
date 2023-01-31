-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

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
  [ testScenario "checks it can flush a transfer type proposal (FA.12)" $ scenario $
      withOriginatedFA12 @variant
        (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) (s { sConfig = (sConfig s) { cPeriod = 35, cProposalFlushLevel = 70 }})) $
        \(admin ::< wallet1 ::< wallet2 ::< Nil') (toPeriod -> period) baseDao _ fa12TokenContract -> do

          let
            transferParam = fa12TokenTransferType (toAddress fa12TokenContract) (toAddress wallet2) (toAddress wallet1)
            proposalMeta = toProposalMetadata @variant $ TransferProposal
                1
                [transferParam]
                []
            proposalSize = metadataSize proposalMeta
            proposeParams = ProposeParams (toAddress wallet1) proposalSize proposalMeta

          withSender wallet1 $
            transfer baseDao $ calling (ep @"Freeze") (#amount :! proposalSize)
          withSender wallet2 $
            transfer baseDao $ calling (ep @"Freeze") (#amount :! 20)

          -- Advance one voting period to a proposing stage.
          startLevel <- getOriginationLevel' @variant baseDao
          advanceToLevel (startLevel + period)

          withSender wallet1 $
            transfer baseDao $ calling (ep @"Propose") proposeParams

          checkBalance' @variant baseDao wallet1 proposalSize

          let
            key1 = makeProposalKey proposeParams
            upvote = NoPermit VoteParam
                { vFrom = toAddress wallet2
                , vVoteType = True
                , vVoteAmount = 20
                , vProposalKey = key1
                }

          -- Advance one voting period to a voting stage.
          advanceToLevel (startLevel + 2*period)
          withSender wallet2 $ transfer baseDao $ calling (ep @"Vote") [upvote]
          -- Advance one voting period to a proposing stage.
          proposalStart <- getProposalStartLevel' @variant baseDao key1
          advanceToLevel (proposalStart + 2*period)
          withSender admin $ transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

          checkBalance' @variant baseDao wallet1 proposalSize
          checkBalance' @variant baseDao wallet2 20

          checkStorage fa12TokenContract
            ( [ (#from :! (toAddress wallet2), #to :! (toAddress wallet1), #value :! 10)
              ])

  , testScenario "checks it can flush a transfer type proposal (#66)" $ scenario $
      withOriginated @variant
        (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) (s { sConfig = (sConfig s) { cPeriod = 35, cProposalFlushLevel = 70 }})) $
        \(admin ::< wallet1 ::< wallet2 ::< Nil') (toPeriod -> period) baseDao dodTokenContract -> do

          let
            proposalMeta = toProposalMetadata @variant $ TransferProposal
                1
                [ tokenTransferType (toAddress dodTokenContract) (toAddress wallet2) (toAddress wallet1)]
                []
            proposalSize = metadataSize proposalMeta
            proposeParams = ProposeParams (toAddress wallet1) proposalSize proposalMeta

          withSender wallet1 $
            transfer baseDao $ calling (ep @"Freeze") (#amount :! proposalSize)
          withSender wallet2 $
            transfer baseDao $ calling (ep @"Freeze") (#amount :! 20)

          -- Advance one voting period to a proposing stage.
          startLevel <- getOriginationLevel' @variant baseDao
          advanceToLevel (startLevel + period)

          withSender wallet1 $
            transfer baseDao $ calling (ep @"Propose") proposeParams

          checkBalance' @variant baseDao wallet1 proposalSize

          let
            key1 = makeProposalKey proposeParams
            upvote = NoPermit VoteParam
                { vFrom = toAddress wallet2
                , vVoteType = True
                , vVoteAmount = 20
                , vProposalKey = key1
                }

          -- Advance one voting period to a voting stage.
          advanceToLevel (startLevel + 2*period)
          withSender wallet2 $ transfer baseDao $ calling (ep @"Vote") [upvote]
          -- Advance one voting period to a proposing stage.
          proposalStart <- getProposalStartLevel' @variant baseDao key1
          advanceToLevel (proposalStart + 2*period)
          withSender admin $ transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

          checkBalance' @variant baseDao wallet1 proposalSize
          checkBalance' @variant baseDao wallet2 20

          checkStorage dodTokenContract
            ( [ [ FA2.TransferItem { tiFrom = (toAddress wallet2), tiTxs = [FA2.TransferDestination { tdTo = toAddress wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
              , [ FA2.TransferItem { tiFrom = (toAddress wallet2), tiTxs = [FA2.TransferDestination { tdTo = toAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 20 }] } ] -- Wallet2 freezes 20 tokens
              , [ FA2.TransferItem { tiFrom = (toAddress wallet1), tiTxs = [FA2.TransferDestination { tdTo = toAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
              ])
  , testScenario "checks it can flush a transfer proposal" $ scenario $
        withOriginated @variant
            (\_ s -> setVariantExtra @variant @"MaxProposalSize" (200 :: Natural) (s { sConfig = (sConfig s) { cPeriod = 35, cProposalFlushLevel = 70 }})) $
          \(admin ::< wallet1 ::< wallet2 ::< Nil') (toPeriod -> period) baseDao _ -> do

            let
              proposalMeta = toProposalMetadata @variant $
                  TransferProposal 1 [ xtzTransferType 3 (toAddress wallet2) ] []
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams (toAddress wallet1) proposalSize proposalMeta

            withSender wallet1 $
              transfer baseDao $ calling (ep @"Freeze") (#amount :! proposalSize)
            withSender wallet2 $
              transfer baseDao $ calling (ep @"Freeze") (#amount :! 10)
            sendXtz baseDao
            startLevel <- getOriginationLevel' @variant baseDao

            -- Advance one voting period to a proposing stage.
            advanceToLevel (startLevel + period)

            withSender wallet1 $
              transfer baseDao $ calling (ep @"Propose") proposeParams
            let key1 = makeProposalKey proposeParams

            checkBalance' @variant baseDao wallet1 proposalSize


            let
              upvote = NoPermit VoteParam
                { vFrom = toAddress wallet2
                , vVoteType = True
                , vVoteAmount = 2
                , vProposalKey = key1
                }

            -- Advance one voting period to a voting stage.
            advanceToLevel (startLevel + 2*period)
            withSender wallet2 $ transfer baseDao $ calling (ep @"Vote") [upvote]
            proposalStart <- getProposalStartLevel' @variant baseDao key1
            advanceToLevel (proposalStart + 2*period + 1)
            withSender admin $ transfer baseDao $ calling (ep @"Flush") (1 :: Natural)

  ]
