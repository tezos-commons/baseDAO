-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum
import Lorentz hiding ((>>))

import Test.Tasty (TestTree, testGroup)
import Time (sec)

import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal
import Ligo.BaseDAO.Types (ProposeParamsL, dynRecUnsafe)
import Test.Ligo.BaseDAO.Common
import Lorentz.Contracts.BaseDAO.Types


vote :: Bool -> ProposalKey pm -> PermitProtected (VoteParam pm)
vote how key =
  NoPermit VoteParam
    { vVoteType = how
    , vVoteAmount = 1
    , vProposalKey = key
    }

upvote, downvote :: ProposalKey pm -> PermitProtected (VoteParam pm)
upvote = vote True
downvote = vote False

data FailureReason = QuorumNotMet | Downvoted

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "LIGO-specific proposal tests:"
    [ nettestScenario "can propose a valid proposal with a fixed fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 42
          let params :: ProposeParamsL = ProposeParams
                { ppFrozenToken = 10
                , ppProposalMetadata = proposalMetadataFromNum 1
                }

          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
          checkTokenBalance frozenTokenId dao proposer 52
          checkTokenBalance unfrozenTokenId dao proposer 48

          -- Check total supply
          let getTotalSupply tokenId consumer = call dao (Call @"Get_total_supply") (mkView tokenId consumer)
          consumer <- originateSimple "consumer" [] contractConsumer
          withSender (AddressResolved proposer) $ getTotalSupply unfrozenTokenId consumer
          checkStorage (AddressResolved $ toAddress consumer) (toVal [148 :: Natural]) -- initial = 200

          consumer2 <- originateSimple "consumer" [] contractConsumer
          withSender (AddressResolved proposer) $ getTotalSupply frozenTokenId consumer2
          checkStorage (AddressResolved $ toAddress consumer2) (toVal [52 :: Natural]) -- initial = 0

    , nettestScenario "cannot propose with insufficient tokens to pay the fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 100

          let params = ProposeParams
                { ppFrozenToken = 1
                , ppProposalMetadata = proposalMetadataFromNum 1
                }
          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
            & expectCustomErrorNoArg #pROPOSAL_INSUFFICIENT_BALANCE

    , nettestScenario "an owner can change the fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 1000

          let params = ProposeParams
                { ppFrozenToken = 0
                , ppProposalMetadata = proposalMetadataFromNum 1
                }
          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
            & expectCustomErrorNoArg #pROPOSAL_INSUFFICIENT_BALANCE

          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 10

          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
          checkTokenBalance frozenTokenId dao proposer 10
          checkTokenBalance unfrozenTokenId dao proposer 90

    , nettestScenario "a non-owner cannot change the fee" $
        uncapsNettest $ do
          ((someone, _), _, dao, _) <- originateLigoDao

          withSender (AddressResolved someone) $
            call dao (Call @"Set_fixed_fee_in_token") 1000
              & expectCustomErrorNoArg #nOT_ADMIN

    , nettestScenario "a proposer is returned a fee after the proposal succeeds" $
        uncapsNettest $ do
          ((proposer, _), (voter, _), dao, admin) <- originateLigoDao

          -- Use 60s for voting period, since in real network by the time we call
          -- the vote entrypoint 30s have already passed.
          withSender (AddressResolved admin) $ do
            call dao (Call @"Set_voting_period") 60
            call dao (Call @"Set_quorum_threshold") 1
            call dao (Call @"Set_fixed_fee_in_token") 42

          key1 <- createSampleProposal 1 proposer dao
          withSender (AddressResolved voter) $
            call dao (Call @"Vote") [upvote key1]

          let expectedFrozen = 42 + 10 -- 'createSampleProposal' freezes 10 tokens
          checkTokenBalance (frozenTokenId) dao proposer expectedFrozen
          checkTokenBalance (unfrozenTokenId) dao proposer (100 - expectedFrozen)

          advanceTime (sec 61)
          withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

          checkTokenBalance (frozenTokenId) dao proposer 0
          checkTokenBalance (unfrozenTokenId) dao proposer 100

    , nettestScenario "the fee is burned if the proposal fails" $
        uncapsNettest $ burnsFeeOnFailure Downvoted

    , nettestScenario "the fee is burned if the proposal doesn't meet the quorum" $
        uncapsNettest $ burnsFeeOnFailure QuorumNotMet
    ],
    mkBaseDaoProposalTests (originateLigoDaoWithConfigDesc dynRecUnsafe)
  ]


burnsFeeOnFailure
  :: forall caps base m. (MonadNettest caps base m)
  => FailureReason -> m ()
burnsFeeOnFailure reason = do
  ((proposer, _), (voter, _), dao, admin) <- originateLigoDao

  -- Use 60s for voting period, since in real network by the time we call
  -- the vote entrypoint 30s have already passed.
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") 1
    call dao (Call @"Set_fixed_fee_in_token") 42

  key1 <- createSampleProposal 1 proposer dao

  case reason of
    Downvoted -> do
      withSender (AddressResolved voter) $
        call dao (Call @"Vote") [downvote key1]
    QuorumNotMet -> return ()

  let expectedFrozen = 42 + 10 -- 'createSampleProposal' freezes 10 tokens
  checkTokenBalance (frozenTokenId) dao proposer expectedFrozen
  checkTokenBalance (unfrozenTokenId) dao proposer (100 - expectedFrozen)

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- Tokens frozen with the proposal are returned, except for the fee and
  -- slash amount. The latter is zero in this case, so we expect 42 tokens
  -- to be burnt
  let expectedBurn = 42
  checkTokenBalance (frozenTokenId) dao proposer 0
  checkTokenBalance (unfrozenTokenId) dao proposer (100 - expectedBurn)
