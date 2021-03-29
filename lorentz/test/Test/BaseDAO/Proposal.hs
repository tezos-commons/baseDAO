-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz.Empty
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.BaseDAO.Types
import Test.BaseDAO.Common
import Test.BaseDAO.Proposal.Config
import Test.BaseDAO.Proposal.Proposal
import Test.BaseDAO.Proposal.Vote

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Proposal :: TestTree
test_BaseDAO_Proposal =
  mkBaseDaoProposalTests
    (originateBaseDaoWithConfig @Integer @Empty ())

-- Note: probably this is worth splitting apart somehow
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
          uncapsNettest $ validProposal originateFn
      , nettestScenario "cannot propose an invalid proposal (rejected)" $
          uncapsNettest $ rejectProposal originateFn
      , nettestScenario "cannot propose a non-unique proposal" $
          uncapsNettest $ nonUniqueProposal originateFn
      ]
  , testGroup "Voter:"
      [ nettestScenario "can vote on a valid proposal" $
          uncapsNettest $ voteValidProposal originateFn
      , nettestScenario "cannot vote non-existing proposal" $
          uncapsNettest $ voteNonExistingProposal originateFn
      , nettestScenario "can vote on multiple proposals" $
          uncapsNettest $ voteMultiProposals originateFn

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated ->
            uncapsNettest $ voteOutdatedProposal originateFn
      ]

  ]
