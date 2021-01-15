-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Assembles tests on all proposals-related logic in a generic manner
-- (can work woth for Lorentz and LIGO).
module BaseDAO.ShareTest.Proposal
  ( mkBaseDaoProposalTests
  ) where

import Universum

import Morley.Nettest

import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.BaseDAO.Types

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal.Bounds
import BaseDAO.ShareTest.Proposal.Config
import BaseDAO.ShareTest.Proposal.Flush
import BaseDAO.ShareTest.Proposal.Proposal
import BaseDAO.ShareTest.Proposal.Vote

-- Note: probably this is worth splitting appart somehow
mkBaseDaoProposalTests
  :: forall config param pm.
     ( ParameterC param pm
     , AllConfigDescsDefined config
     , ProposalMetadataFromNum pm
     , HasCallStack
     )
  => ( forall m. Monad m =>
       ConfigDesc config -> OriginateFn param (NettestT m)
     )
  -> TestTree
mkBaseDaoProposalTests originateFn =
  -- Note: we have to make sure that each individual test suite gets
  -- callstack this function's caller, otherwise attempts to print
  -- failures will fail with "openFile: does not exist".
  -- This should be resolved in
  -- https://gitlab.com/morley-framework/morley/-/issues/528.
  withFrozenCallStack $
  testGroup "BaseDAO propose/vote entrypoints tests:"
  [ testGroup "Proposal creator:"
      [ nettestScenario "can propose a valid proposal" $
          uncapsNettest $ validProposal True originateFn
      , nettestScenario "cannot propose an invalid proposal (rejected)" $
          uncapsNettest $ rejectProposal True originateFn
      , nettestScenario "cannot propose with insufficient tokens" $
          uncapsNettest $ insufficientTokenProposal True originateFn
      , nettestScenario "cannot propose a non-unique proposal" $
          uncapsNettest $ nonUniqueProposal True originateFn
      ]
  , testGroup "Voter:"
      [ nettestScenario "can vote on a valid proposal" $
          uncapsNettest $ voteValidProposal True originateFn
      , nettestScenario "cannot vote non-existing proposal" $
          uncapsNettest $ voteNonExistingProposal True originateFn
      , nettestScenario "can vote on multiple proposals" $
          uncapsNettest $ voteMultiProposals True originateFn

      , nettestScenario "cannot vote if the vote amounts exceeds token balance" $
          uncapsNettest $ insufficientTokenVote True originateFn
      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated ->
            uncapsNettest $ voteOutdatedProposal True originateFn
      , testGroup "Permit:"
          [ nettestScenario "can vote from another user's behalf" $
              uncapsNettest $ voteWithPermit True originateFn
          , nettestScenario "counter works properly in permits" $
              uncapsNettest $ voteWithPermitNonce True originateFn
          ]
      ]
  , testGroup "Admin:"
      [ nettestScenario "can set voting period"  $
          uncapsNettest $ setVotingPeriod True originateFn

      , nettestScenario "can set quorum threshold" $
          uncapsNettest $ setQuorumThreshold True originateFn

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can flush proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposals True originateFn
      , nettestScenarioOnEmulator "can flush 2 proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposalsWithAnAmount True originateFn
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to not meeting quorum_threshold" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalQuorum True originateFn
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to negative votes" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalNegativeVotes True originateFn
      , nettestScenario "flush should not affecting ongoing proposals" $
          uncapsNettest $ flushNotAffectOngoingProposals True
            originateFn
      , nettestScenarioOnEmulator "flush with bad 'cRejectedProposalReturnValue'" $
          \_emulated ->
            uncapsNettest $ flushWithBadConfig True originateFn
      -- TODO [#15]: admin burn proposer token and test "flush"

      -- TODO [#38]: Improve this when contract size is smaller
      , nettestScenarioOnEmulator "flush and run decision lambda" $
          \_emulated ->
            uncapsNettest $ flushDecisionLambda True originateFn
      , nettestScenarioOnEmulator "can drop proposals" $
          \_emulated ->
            uncapsNettest $ dropProposal True originateFn
      ]
  , testGroup "Bounded Value"
      [ nettestScenario "bounded value on proposals" $
          uncapsNettest $ proposalBoundedValue True originateFn
      , nettestScenario "bounded value on votes" $
          uncapsNettest $ votesBoundedValue True originateFn
      , nettestScenario "bounded range on quorum_threshold" $
          uncapsNettest $ quorumThresholdBound True originateFn
      , nettestScenario "bounded range on voting_period" $
          uncapsNettest $ votingPeriodBound True originateFn
      ]
  ]
