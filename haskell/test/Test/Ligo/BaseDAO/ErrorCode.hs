-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.ErrorCode
  ( test_ErrorCodes
  ) where

import Universum hiding (view)

import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Ligo.BaseDAO.ErrorCodes

test_ErrorCodes :: TestTree
test_ErrorCodes = testGroup "FA2 off-chain views"
  [ testCase "Ensure the error codes are as expected" $ do
      notAdmin @?= 100
      notPendingAdmin @?= 101
      failProposalCheck @?= 102
      proposalNotExist @?= 103
      votingStageOver @?= 104
      maxProposalsReached @?= 105
      forbiddenXtz @?= 107
      proposalNotUnique @?= 108
      missigned @?= 109
      unpackingFailed @?= 110
      unpackingProposalMetadataFailed @?= 111
      missingValue @?= 112
      notProposingStage @?= 113
      notEnoughFrozenTokens @?= 114
      badTokenContract @?= 115
      badViewContract @?= 116
      dropProposalConditionNotMet @?= 117
      expiredProposal @?= 118
      emptyFlush @?= 119
      notDelegate @?= 120
      failDecisionLambda @?= 121
      unstakeInvalidProposal @?= 122
      voterDoesNotExist @?= 123
      badState @?= 300
  ]
