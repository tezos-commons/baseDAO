-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoint logic for testing the  behaviour
-- associated with quorum threshold and its dynamic updates.
module Test.Ligo.BaseDAO.Proposal.Quorum
  ( proposalIsRejectedIfNoQuorum
  , proposalSucceedsIfUpVotesGtDownvotesAndQuorum
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

proposalIsRejectedIfNoQuorum
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
proposalIsRejectedIfNoQuorum = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe
      ((  ConfigDesc $ mkQuorumThreshold 1 20)
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc (FixedFee 42))
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 1800 })
      )
  let proposer = dodOwner1
  let voter = dodOwner2
  let dao = dodDao
  let admin = dodAdmin

  withSender voter $
    call dao (Call @"Freeze") (#amount .! 28)

  withSender proposer $
    call dao (Call @"Freeze") (#amount .! 42)

  withSender proposer $
    call dao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  key1 <- createSampleProposal 1 proposer dao
  -- Advance one voting period to a voting stage.
  advanceLevel 60
  let vote_ =
        NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 13
            -- The minimum votes that is required to pass is 280 * 1/20  = 14.
          , vProposalKey = key1
          }
  withSender voter $
    call dao (Call @"Vote") [vote_]

  let expectedFrozen = 100 + 42 + 10
  checkTokenBalance frozenTokenId dao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  withSender admin $ call dao (Call @"Flush") 100

  checkTokenBalance frozenTokenId dao proposer 110 -- We expect 42 tokens to have burned

proposalSucceedsIfUpVotesGtDownvotesAndQuorum
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
proposalSucceedsIfUpVotesGtDownvotesAndQuorum = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe
      ((  ConfigDesc $ mkQuorumThreshold 1 20)
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc (FixedFee 42))
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 1800 })
      )
  let proposer = dodOwner1
  let voter = dodOwner2
  let dao = dodDao
  let admin = dodAdmin

  withSender voter $
    call dao (Call @"Freeze") (#amount .! 28)

  withSender proposer $
    call dao (Call @"Freeze") (#amount .! 42)

  withSender proposer $
    call dao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  key1 <- createSampleProposal 1 proposer dao
  -- Advance one voting period to a voting stage.
  advanceLevel 60
  let vote_ =
        NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 14
            -- The minimum votes that is required to pass is 280 * 1/20  = 14.
          , vProposalKey = key1
          }
  withSender voter $
    call dao (Call @"Vote") [vote_]

  let expectedFrozen = 100 + 42 + 10
  checkTokenBalance frozenTokenId dao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  withSender admin $ call dao (Call @"Flush") 100

  checkTokenBalance frozenTokenId dao proposer 152

