-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE ApplicativeDo #-}

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import Test.Cleveland
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config
import Test.Ligo.BaseDAO.Proposal.Flush
import Test.Ligo.BaseDAO.Proposal.Propose
import Test.Ligo.BaseDAO.Proposal.Quorum
import Test.Ligo.BaseDAO.Proposal.Tokens
import Test.Ligo.BaseDAO.Proposal.Vote

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "Proposal creator:"
      [ testScenario "BaseDAO - can propose a valid proposal" $ scenario $
          validProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot propose an invalid proposal (rejected)" $ scenario $
          rejectProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot propose a non-unique proposal" $ scenario $
          nonUniqueProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot propose same proposal even after dropping original one" $ scenario $
          nonUniqueProposalEvenAfterDrop (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot propose in a non-proposal period" $ scenario $
          nonProposalPeriodProposal (originateLigoDaoWithConfigDesc @'Base ())

      ]

  , testGroup "Voter:"
      [ testScenario "can vote on a valid proposal" $ scenario $
          voteValidProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot vote on a deleted proposal" $ scenario $
          voteDeletedProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot vote non-existing proposal" $ scenario $
          voteNonExistingProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "can vote on multiple proposals" $ scenario $
          voteMultiProposals (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot vote on outdated proposal" $ scenario $
          voteOutdatedProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "proposal track votes" $ scenario $
          proposalCorrectlyTrackVotes (originateLigoDaoWithConfigDesc @'Base ())

      ]


  , testScenario "cannot vote if the vote amounts exceeds token balance" $ scenario $
      insufficientTokenVote (originateLigoDaoWithConfigDesc @'Base ())

  -- Note: When checking storage, we need to split the test into 2 (emulator and network) as demonstrated below:
  , testScenario "cannot propose with insufficient tokens " $ scenario $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc @'Base ()) (\addr -> (length . sProposalKeyListSortByDateRPC . fsStorageRPC) <$> getStorageRPC (TAddress addr))

  , testGroup "Permit:"
      [ testScenario "can vote from another user behalf" $ scenario $
          voteWithPermit (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "counter works properly in permits" $ scenario $
          voteWithPermitNonce (originateLigoDaoWithConfigDesc @'Base ())

      ]
  , testGroup "Admin:"
      [ testScenario "can flush proposals that got accepted" $ scenario $
          flushAcceptedProposals (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "can flush 2 proposals that got accepted" $ scenario $
          flushAcceptedProposalsWithAnAmount
            (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "can flush proposals that got rejected due to not meeting quorum_threshold" $ scenario $
          flushRejectProposalQuorum (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "can flush proposals that got rejected due to negative votes" $ scenario $
          flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "flush should not affect proposals that cannot be flushed yet" $ scenario $
          flushProposalFlushTimeNotReach (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "flush should fail on expired proposals" $ scenario $
          flushFailOnExpiredProposal (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "flush with bad cRejectedProposalSlashValue" $ scenario $
          flushWithBadConfig (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "flush and run decision lambda" $ scenario $
          flushDecisionLambda (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "empty flush calls are rejected" $ scenario $
          flushNotEmpty (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "can drop proposals, only when allowed" $ scenario $
          dropProposal (originateLigoDaoWithConfigDesc @'Base ())

      ]

  , testGroup "Bounded Value"
      [ testScenario "bounded value on proposals" $ scenario $
          proposalBoundedValue (originateLigoDaoWithConfigDesc @'Base ())
      ]

  , testGroup "Freeze-Unfreeze"
      [ testScenario "can freeze tokens " $ scenario $
          freezeTokens (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot unfreeze tokens from the same period" $ scenario $
          cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "cannot unfreeze staked tokens" $ scenario $
          cannotUnfreezeStakedTokens (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "can unfreeze tokens from the previous period" $ scenario $
          canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "correctly track freeze history" $ scenario $
          checkFreezeHistoryTracking (originateLigoDaoWithConfigDesc @'Base ())

      , testScenario "tokens are unstaked correctly, only when possible" $ scenario $
          unstakeVote (originateLigoDaoWithConfigDesc @'Base ())
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
          checkQuorumThresholdDynamicUpdate (originateLigoDaoWithConfigDesc @'Base ())
      , testScenario "updates quorum-threshold correctly within upper bounds" $ scenario $
          checkQuorumThresholdDynamicUpdateUpperBound (originateLigoDaoWithConfigDesc @'Base ())
      , testScenario "updates quorum-threshold correctly within lower bounds" $ scenario $
          checkQuorumThresholdDynamicUpdateLowerBound (originateLigoDaoWithConfigDesc @'Base ())
      , testScenario "proposal saves quorum for cycle" $ scenario $
          checkProposalSavesQuorum (originateLigoDaoWithConfigDesc @'Base ())
      ]
  ]
