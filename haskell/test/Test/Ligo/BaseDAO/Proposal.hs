-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE ApplicativeDo #-}

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Lorentz hiding (assert, (>>))

import Test.Cleveland
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Flush
import Test.Ligo.BaseDAO.Proposal.Propose
import Test.Ligo.BaseDAO.Proposal.Quorum
import Test.Ligo.BaseDAO.Proposal.Tokens
import Test.Ligo.BaseDAO.Proposal.Vote

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "Proposal creator:"
      [ testScenario "BaseDAO - can propose a valid proposal" $ scenario $
          validProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot propose an invalid proposal (rejected)" $ scenario $
          rejectProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot propose a non-unique proposal" $ scenario $
          nonUniqueProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot propose same proposal even after dropping original one" $ scenario $
          nonUniqueProposalEvenAfterDrop (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot propose in a non-proposal period" $ scenario $
          nonProposalPeriodProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Voter:"
      [ testScenario "can vote on a valid proposal" $ scenario $
          voteValidProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot vote on a deleted proposal" $ scenario $
          voteDeletedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot vote non-existing proposal" $ scenario $
          voteNonExistingProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "can vote on multiple proposals" $ scenario $
          voteMultiProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot vote on outdated proposal" $ scenario $
          voteOutdatedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "proposal track votes" $ scenario $
          proposalCorrectlyTrackVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]


  , testScenario "cannot vote if the vote amounts exceeds token balance" $ scenario $
      insufficientTokenVote (originateLigoDaoWithConfigDesc dynRecUnsafe)

  -- Note: When checking storage, we need to split the test into 2 (emulator and network) as demonstrated below:
  , testScenario "cannot propose with insufficient tokens " $ scenario $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

  , testGroup "Permit:"
      [ testScenario "can vote from another user behalf" $ scenario $
          voteWithPermit (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "counter works properly in permits" $ scenario $
          voteWithPermitNonce (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]
  , testGroup "Admin:"
      [ testScenario "can flush proposals that got accepted" $ scenario $
          flushAcceptedProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "can flush 2 proposals that got accepted" $ scenario $
          flushAcceptedProposalsWithAnAmount
            (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "can flush proposals that got rejected due to not meeting quorum_threshold" $ scenario $
          flushRejectProposalQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "can flush proposals that got rejected due to negative votes" $ scenario $
          flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "flush should not affect proposals that cannot be flushed yet" $ scenario $
          flushProposalFlushTimeNotReach (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "flush should fail on expired proposals" $ scenario $
          flushFailOnExpiredProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "flush with bad cRejectedProposalSlashValue" $ scenario $
          flushWithBadConfig (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "flush and run decision lambda" $ scenario $
          flushDecisionLambda (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "empty flush calls are rejected" $ scenario $
          flushNotEmpty (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "can drop proposals, only when allowed" $ scenario $
          dropProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Freeze-Unfreeze"
      [ testScenario "can freeze tokens " $ scenario $
          freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot unfreeze tokens from the same period" $ scenario $
          cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "cannot unfreeze staked tokens" $ scenario $
          cannotUnfreezeStakedTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "can unfreeze tokens from the previous period" $ scenario $
          canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "correctly track freeze history" $ scenario $
          checkFreezeHistoryTracking (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , testScenario "tokens are unstaked correctly, only when possible" $ scenario $
          unstakeVote (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

 , testGroup "LIGO-specific proposal tests:"
    [ testScenario "can propose a valid proposal with a fixed fee" $ scenario $
        validProposalWithFixedFee

    , testScenario "cannot propose with insufficient tokens to pay the fee" $ scenario
       cannotProposeWithInsufficientTokens

    , testScenario "a proposer is returned a fee after the proposal succeeds" $ scenario $
       proposerIsReturnedFeeAfterSucceeding

    , testScenario "a proposal is rejected if upvotes > downvotes and quorum threshold is not met" $ scenario $
        proposalIsRejectedIfNoQuorum

    , testScenario "a proposal succeeds if upvotes > downvotes and quorum threshold is met" $ scenario $
        proposalSucceedsIfUpVotesGtDownvotesAndQuorum

    , testScenario "the fee is burned if the proposal fails" $ scenario $
        burnsFeeOnFailure Downvoted

    , testScenario "the fee is burned if the proposal doesn't meet the quorum" $ scenario $
        burnsFeeOnFailure QuorumNotMet

    , testScenario "the frozen tokens are correctly unstaked when address cast multiple votes" $ scenario $
        unstakesTokensForMultipleVotes
    ]

  , testGroup "QuorumThreshold Updates"
      [ testScenario "updates quorum-threshold correctly" $ scenario $
          checkQuorumThresholdDynamicUpdate (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , testScenario "updates quorum-threshold correctly within upper bounds" $ scenario $
          checkQuorumThresholdDynamicUpdateUpperBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , testScenario "updates quorum-threshold correctly within lower bounds" $ scenario $
          checkQuorumThresholdDynamicUpdateLowerBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , testScenario "proposal saves quorum for cycle" $ scenario $
          checkProposalSavesQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]
  ]
