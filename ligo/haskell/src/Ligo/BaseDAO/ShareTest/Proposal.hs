-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Assembles tests on all proposals-related logic in a generic manner
-- (can work woth for Lorentz and LIGO).
module Ligo.BaseDAO.ShareTest.Proposal
  ( mkBaseDaoProposalTests
  ) where

import Universum

import Morley.Nettest

import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.ShareTest.Common
import Ligo.BaseDAO.ShareTest.Proposal.Config
import Ligo.BaseDAO.ShareTest.Proposal.Proposal
import Ligo.BaseDAO.ShareTest.Proposal.Vote
import Ligo.BaseDAO.Types

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
      [ nettestScenario "BaseDAO - can propose a valid proposal" $
          uncapsNettest $ validProposal True originateFn
      , nettestScenario "cannot propose an invalid proposal (rejected)" $
          uncapsNettest $ rejectProposal True originateFn
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

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated ->
            uncapsNettest $ voteOutdatedProposal True originateFn
      ]

  ]
