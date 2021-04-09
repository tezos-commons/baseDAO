-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum
import Lorentz hiding (assert, (>>))

import Time (sec)

import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario, nettestScenarioOnEmulator)
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Bounds
import Test.Ligo.BaseDAO.Proposal.Config
import Test.Ligo.BaseDAO.Proposal.Proposal
import Test.Ligo.BaseDAO.Proposal.Vote

vote :: Bool -> ProposalKey -> PermitProtected VoteParam
vote how key =
  NoPermit VoteParam
    { vVoteType = how
    , vVoteAmount = 1
    , vProposalKey = key
    }

upvote, downvote :: ProposalKey -> PermitProtected VoteParam
upvote = vote True
downvote = vote False

data FailureReason = QuorumNotMet | Downvoted

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "Proposal creator:"
      [ nettestScenario "BaseDAO - can propose a valid proposal" $
          uncapsNettest $ validProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot propose an invalid proposal (rejected)" $
          uncapsNettest $ rejectProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot propose a non-unique proposal" $
          uncapsNettest $ nonUniqueProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot propose in a non-proposal period" $
          uncapsNettest $ nonProposalPeriodProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Voter:"
      [ nettestScenario "can vote on a valid proposal" $
          uncapsNettest $ voteValidProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot vote non-existing proposal" $
          uncapsNettest $ voteNonExistingProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "can vote on multiple proposals" $
          uncapsNettest $ voteMultiProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated ->
            uncapsNettest $ voteOutdatedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]


  , nettestScenario "cannot vote if the vote amounts exceeds token balance" $
      uncapsNettest $ insufficientTokenVote (originateLigoDaoWithConfigDesc dynRecUnsafe)

  , nettestScenario "cannot propose with insufficient tokens" $
      uncapsNettest $ insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

  , testGroup "Permit:"
      [ nettestScenario "can vote from another user behalf" $
          uncapsNettest $ voteWithPermit (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "counter works properly in permits" $
          uncapsNettest $ voteWithPermitNonce (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]
  , testGroup "Admin:"
      [ nettestScenario "can set voting period"  $
          uncapsNettest $ setVotingPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "can set quorum threshold" $
          uncapsNettest $ setQuorumThreshold (originateLigoDaoWithConfigDesc dynRecUnsafe)

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can flush proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush 2 proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposalsWithAnAmount (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to not meeting quorum_threshold" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to negative votes" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "flush should not affecting ongoing proposals" $
          uncapsNettest $ flushNotAffectOngoingProposals
            (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "flush with bad cRejectedProposalReturnValue" $
          \_emulated ->
            uncapsNettest $ flushWithBadConfig (originateLigoDaoWithConfigDesc dynRecUnsafe)
      -- TODO [#15]: admin burn proposer token and test "flush"

      -- TODO [#38]: Improve this when contract size is smaller
      , nettestScenarioOnEmulator "flush and run decision lambda" $
          \_emulated ->
            uncapsNettest $ flushDecisionLambda (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can drop proposals" $
          \_emulated ->
            uncapsNettest $ dropProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Bounded Value"
      [ nettestScenario "bounded value on proposals" $
          uncapsNettest $ proposalBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "bounded value on votes" $
          uncapsNettest $ votesBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "bounded range on quorum_threshold" $
          uncapsNettest $ quorumThresholdBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "bounded range on voting_period" $
          uncapsNettest $ votingPeriodBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Freeze-Unfreeze"
      [ nettestScenario "can freeze tokens" $
          uncapsNettest $ freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "cannot unfreeze tokens from the same period" $
          uncapsNettest $ cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "can unfreeze tokens from the previous period" $
          uncapsNettest $ canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "handle voting period change" $
          uncapsNettest $ canHandleVotingPeriodChange (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "handle voting period change" $
          uncapsNettest $ votingPeriodChange (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

 , testGroup "LIGO-specific proposal tests:"
    [ nettestScenario "can propose a valid proposal with a fixed fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 42
          let params = ProposeParams
                { ppFrozenToken = 10
                , ppProposalMetadata = proposalMetadataFromNum 1
                }

          withSender (AddressResolved proposer) $
            call dao (Call @"Freeze") (#amount .! 52)
          advanceTime (sec 10)

          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
          checkTokenBalance frozenTokenId dao proposer 52
          checkTokenBalance unfrozenTokenId dao proposer 48

          -- Check total supply
          withSender (AddressResolved proposer) $
            call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
              & expectError dao (VoidResult (148 :: Natural)) -- initial = 200

          withSender (AddressResolved proposer) $
            call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
              & expectError dao (VoidResult (52 :: Natural)) -- initial = 0

    , nettestScenario "cannot propose with insufficient tokens to pay the fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 100

          withSender (AddressResolved proposer) $
            call dao (Call @"Freeze") (#amount .! 52)
          advanceTime (sec 10)

          let params = ProposeParams
                { ppFrozenToken = 1
                , ppProposalMetadata = proposalMetadataFromNum 1
                }
          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
            & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

    , nettestScenario "an owner can change the fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 1000

          let params = ProposeParams
                { ppFrozenToken = 0
                , ppProposalMetadata = proposalMetadataFromNum 1
                }

          withSender (AddressResolved proposer) $
            call dao (Call @"Freeze") (#amount .! 52)
          advanceTime (sec 10)

          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
            & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 10

          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
          checkTokenBalance frozenTokenId dao proposer 52
          checkTokenBalance unfrozenTokenId dao proposer 48

    , nettestScenario "a non-owner cannot change the fee" $
        uncapsNettest $ do
          ((someone, _), _, dao, _) <- originateLigoDao

          withSender (AddressResolved someone) $
            call dao (Call @"Set_fixed_fee_in_token") 1000
              & expectCustomErrorNoArg #nOT_ADMIN dao

    , nettestScenario "a proposer is returned a fee after the proposal succeeds" $
        uncapsNettest $ do
          ((proposer, _), (voter, _), dao, admin) <- originateLigoDao

          -- Use 60s for voting period, since in real network by the time we call
          -- the vote entrypoint 30s have already passed.
          withSender (AddressResolved admin) $ do
            call dao (Call @"Set_voting_period") 60
            call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 100
            call dao (Call @"Set_fixed_fee_in_token") 42

          withSender (AddressResolved voter) $
            call dao (Call @"Freeze") (#amount .! 1)

          withSender (AddressResolved proposer) $
            call dao (Call @"Freeze") (#amount .! 42)

          advanceTime (sec 61)
          key1 <- createSampleProposal 1 60 proposer dao
          advanceTime (sec 60)
          withSender (AddressResolved voter) $
            call dao (Call @"Vote") [upvote key1]

          let expectedFrozen = 42 + 10 -- 'createSampleProposal' freezes 10 tokens
          checkTokenBalance (frozenTokenId) dao proposer expectedFrozen
          checkTokenBalance (unfrozenTokenId) dao proposer (100 - expectedFrozen)

          advanceTime (sec 60)
          withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

          checkTokenBalance (frozenTokenId) dao proposer 52
          checkTokenBalance (unfrozenTokenId) dao proposer 48

    , nettestScenario "the fee is burned if the proposal fails" $
        uncapsNettest $ burnsFeeOnFailure Downvoted

    , nettestScenario "the fee is burned if the proposal doesn't meet the quorum" $
        uncapsNettest $ burnsFeeOnFailure QuorumNotMet
    ]
  ]

nonProposalPeriodProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonProposalPeriodProposal originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10)

  advanceTime (sec 10)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorNoArg #nOT_PROPOSING_PERIOD dao

freezeTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
freezeTokens originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 10

burnsFeeOnFailure
  :: forall caps base m. (MonadNettest caps base m)
  => FailureReason -> m ()
burnsFeeOnFailure reason = do
  ((proposer, _), (voter, _), dao, admin) <- originateLigoDao

  -- Use 60s for voting period, since in real network by the time we call
  -- the vote entrypoint 30s have already passed.
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 100
    call dao (Call @"Set_fixed_fee_in_token") 42

  withSender (AddressResolved proposer) $
    call dao (Call @"Freeze") (#amount .! 42)

  withSender (AddressResolved voter) $
    call dao (Call @"Freeze") (#amount .! 1)

  advanceTime (sec 61)
  key1 <- createSampleProposal 1 60 proposer dao

  advanceTime (sec 60)
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

  -- Tokens frozen with the proposal are returned as unstaked (but still
  -- frozen), except for the fee and slash amount. The latter is zero in this
  -- case, so we expect 42 tokens to be burnt
  let expectedBurn = 42
  checkTokenBalance (frozenTokenId) dao proposer (52 - expectedBurn)
  checkTokenBalance (unfrozenTokenId) dao proposer 48

cannotUnfreezeFromSamePeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
cannotUnfreezeFromSamePeriod originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 10

  -- Cannot unfreeze in the same period
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

canUnfreezeFromPreviousPeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
canUnfreezeFromPreviousPeriod originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 10

  advanceTime (sec 15)

  -- Cannot unfreeze in the same period
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 0
  checkTokenBalance unfrozenTokenId dao owner1 100

canHandleVotingPeriodChange
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
canHandleVotingPeriodChange originateFn = do
  -- Initial voting period is 10 sec
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 10

  advanceTime (sec 5)

  withSender (AddressResolved admin) $
    call dao (Call @"Set_voting_period") 15

  advanceTime (sec 10)
  -- Since we count voting period from the last time it was changed, we need
  -- additional 15 seconds to wait before the next period starts. Here only
  -- 10 seconds is advanced, so we are in the current period itself.

  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

  advanceTime (sec 5)
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)

insufficientTokenProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
insufficientTokenProposal originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

insufficientTokenVote
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
insufficientTokenVote originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig
  advanceTime (sec 120)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 100)

  -- Create sample proposal
  key1 <- createSampleProposal 1 120 owner1 dao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 51
            , vProposalKey = key1
           }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 50
            , vProposalKey = key1
            }
        ]
  advanceTime (sec 120)

  withSender (AddressResolved owner2) $ call dao (Call @"Vote") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

voteWithPermit
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteWithPermit originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig
  advanceTime (sec 120)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 2)

  -- Create sample proposal
  key1 <- createSampleProposal 1 120 owner1 dao

  params <- permitProtect (AddressResolved owner1) =<< addDataToSign dao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 120)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dao owner1 12

voteWithPermitNonce
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteWithPermitNonce originateFn = do

  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  advanceTime (sec 120)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 50)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 50)

  -- Create sample proposal
  key1 <- createSampleProposal 1 125 owner1 dao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 120)
  -- Going to try calls with different nonces
  signed1@(_          , _) <- addDataToSign dao (Nonce 0) voteParam
  signed2@(dataToSign2, _) <- addDataToSign dao (Nonce 1) voteParam
  signed3@(_          , _) <- addDataToSign dao (Nonce 2) voteParam

  params1 <- permitProtect (AddressResolved owner1) signed1
  params2 <- permitProtect (AddressResolved owner1) signed2
  params3 <- permitProtect (AddressResolved owner1) signed3

  withSender (AddressResolved owner2) $ do
    -- Good nonce
    call dao (Call @"Vote") [params1]

    -- Outdated nonce
    call dao (Call @"Vote") [params1]
      & expectCustomError #mISSIGNED dao (checkedCoerce $ lPackValue dataToSign2)

    -- Nonce from future
    call dao (Call @"Vote") [params3]
      & expectCustomError #mISSIGNED dao (checkedCoerce $ lPackValue dataToSign2)

    -- Good nonce after the previous successful entrypoint call
    call dao (Call @"Vote") [params2]

  -- Check counter
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_vote_permit_counter") (mkVoid ())
      & expectError dao (VoidResult (2 :: Natural))

flushNotAffectOngoingProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushNotAffectOngoingProposals originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig


  -- Note: Cannot set to few seconds, since in real network, each
  -- calls takes some times to run. 20 seconds seem to be the ideal.
  withSender (AddressResolved admin) $
    call dao (Call @"Set_voting_period") (2 * 60)
  advanceTime (sec 120)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 20)

  advanceTime (sec 125)

  _key1 <- createSampleProposal 1 0 owner1 dao
  _key2 <- createSampleProposal 2 0 owner1 dao
  advanceTime (sec 125)
  withSender (AddressResolved admin) $
    call dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  -- checkIfAProposalExist (key2 :: ByteString) dao

flushAcceptedProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushAcceptedProposals originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig

  -- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 100

  advanceTime (sec 60)
  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 3)

  -- Accepted Proposals
  key1 <- createSampleProposal 1 65 owner1 dao
  advanceTime (sec 65)

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }
      downvote' = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  withSender (AddressResolved owner2) $
    call dao (Call @"Vote") [upvote', downvote']

  -- Checking balance of proposer and voters
  checkTokenBalance (frozenTokenId) dao owner1 10
  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance (frozenTokenId) dao owner1 10
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- proposer

  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97 -- voter

  -- Check total supply (After flush, no changes are made.)
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
      & expectError dao (VoidResult (187 :: Natural)) -- initial = 200

  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError dao (VoidResult (13 :: Natural)) -- initial = 0


flushAcceptedProposalsWithAnAmount
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushAcceptedProposalsWithAnAmount originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig

  advanceTime (sec 10)
  -- Accepted Proposals
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 40)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2)

  advanceTime (sec 15)
  key1 <- createSampleProposal 1 0 owner1 dao
  key2 <- createSampleProposal 2 0 owner1 dao
  advanceTime (sec 1)
  key3 <- createSampleProposal 3 0 owner1 dao

  let vote' key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key
        }

  advanceTime (sec 21)

  key4 <- createSampleProposal 4 0 owner1 dao

  checkTokenBalance (frozenTokenId) dao owner1 40
  checkTokenBalance (unfrozenTokenId) dao owner1 60

  advanceTime (sec 10)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 2

  -- Proposals are flushed
  withSender (AddressResolved owner2) $ do
    call dao (Call @"Vote") [vote' key1]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER dao
    call dao (Call @"Vote") [vote' key2]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER dao

    -- Proposal is over but not affected
    call dao (Call @"Vote") [vote' key3]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER dao

    -- Proposal is not yet over
    call dao (Call @"Vote") [vote' key4]

  -- Only 2 proposals are flush, so only 20 tokens are unfrozen back.
  checkTokenBalance (frozenTokenId) dao owner1 40
  checkTokenBalance (unfrozenTokenId) dao owner1 60

flushRejectProposalQuorum
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushRejectProposalQuorum originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn configWithRejectedProposal

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 3 5
  advanceTime (sec 60)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 5)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 65 owner1 dao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        ]
  advanceTime (sec 60)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") votes

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance (frozenTokenId) dao owner1 5
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (frozenTokenId) dao owner2 5
  checkTokenBalance (unfrozenTokenId) dao owner2 95 -- voter

flushRejectProposalNegativeVotes
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushRejectProposalNegativeVotes originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn configWithRejectedProposal

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 3 100

  advanceTime (sec 60)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 3)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 65 owner1 dao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        ]
  advanceTime (sec 60)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") votes

  -- Check proposer balance
  checkTokenBalance (frozenTokenId) dao owner1 10

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance (frozenTokenId) dao owner1 5
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97 -- voter

flushWithBadConfig
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushWithBadConfig originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn badRejectedValueConfig

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 2

  advanceTime (sec 60)
  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 3)
  key1 <- createSampleProposal 1 65 owner1 dao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  advanceTime (sec 60)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote']

  checkTokenBalance (unfrozenTokenId) dao owner1 90
  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- slash all frozen values
  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97

flushDecisionLambda
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushDecisionLambda originateFn = do
  consumer <- originateSimple "consumer" [] (contractConsumer)
  ((owner1, _), (owner2, _), dao, admin) <- originateFn (decisionLambdaConfig consumer)

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 100

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 1)
  advanceTime (sec 65)
  key1 <- createSampleProposal 1 60 owner1 dao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  advanceTime (sec 60)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote']

  advanceTime (sec 60)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  results <- fromVal <$> getStorage (AddressResolved $ toAddress consumer)
  assert (results == (#proposer <.!> [owner1]))
    "Unexpected accepted proposals list"

dropProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
dropProposal originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn badRejectedValueConfig

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 20
    call dao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 50
  advanceTime (sec 25)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 30)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2)
  advanceTime (sec 20)

  key1 <- createSampleProposal 1 0 owner1 dao
  key2 <- createSampleProposal 2 0 owner1 dao

  advanceTime (sec 20)
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key
        }
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params key1]
  advanceTime (sec 20)

  key3 <- createSampleProposal 3 0 owner1 dao

  withSender (AddressResolved admin) $ do
    call dao (Call @"Drop_proposal") key1
    call dao (Call @"Drop_proposal") key2
      & expectCustomErrorNoArg #fAIL_DROP_PROPOSAL_NOT_ACCEPTED dao
    call dao (Call @"Drop_proposal") key3
      & expectCustomErrorNoArg #fAIL_DROP_PROPOSAL_NOT_OVER dao

  -- 30 tokens are frozen in total, but 10 tokens are returned after drop_proposal
  checkTokenBalance (frozenTokenId) dao owner1 30
  checkTokenBalance (unfrozenTokenId) dao owner1 70

proposalBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
proposalBoundedValue originateFn = do
  ((owner1, _), _, dao, _) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts{ cmMaxProposals = Just 1 }
    )
  advanceTime (sec 11)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 20)

  advanceTime (sec 10)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ do
    call dao (Call @"Propose") params
    call dao (Call @"Propose") params
      & expectCustomErrorNoArg #mAX_PROPOSALS_REACHED dao

votesBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
votesBoundedValue originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn
    ( voteConfig >>-
      ConfigDesc configConsts{ cmMaxVotes = Just 1 }
    )
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 2)

  advanceTime (sec 120)
  key1 <- createSampleProposal 1 120 owner2 dao
  let upvote' = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
      downvote' = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  advanceTime (sec 120)
  withSender (AddressResolved owner1) $ do
    call dao (Call @"Vote") [downvote']
    call dao (Call @"Vote") [upvote']
      & expectCustomErrorNoArg #mAX_VOTES_REACHED dao

quorumThresholdBound
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
quorumThresholdBound originateFn = do
  (_, _, dao, admin) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts
        { cmMinQuorumThreshold = Just $ QuorumThreshold 1 100
        , cmMaxQuorumThreshold = Just $ QuorumThreshold 2 100
        }
    )
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_quorum_threshold") (QuorumThreshold 1 100)
    call dao (Call @"Set_quorum_threshold") (QuorumThreshold 2 100)
    call dao (Call @"Set_quorum_threshold") (QuorumThreshold 0 100)
      & expectCustomErrorNoArg #oUT_OF_BOUND_QUORUM_THRESHOLD dao
    call dao (Call @"Set_quorum_threshold") (QuorumThreshold 3 100)
      & expectCustomErrorNoArg #oUT_OF_BOUND_QUORUM_THRESHOLD dao

votingPeriodBound
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
votingPeriodBound originateFn = do
  (_, _, dao, admin) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts
        { cmMinVotingPeriod = Just 1
        , cmMaxVotingPeriod = Just 2
        }
    )
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 1
    call dao (Call @"Set_voting_period") 2
    call dao (Call @"Set_voting_period") 0
      & expectCustomErrorNoArg #oUT_OF_BOUND_VOTING_PERIOD dao
    call dao (Call @"Set_voting_period") 3
      & expectCustomErrorNoArg #oUT_OF_BOUND_VOTING_PERIOD dao

votingPeriodChange
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
votingPeriodChange originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig
  advanceTime (sec 11)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 10)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10)

  advanceTime (sec 10)

  -- Create sample proposal
  key1 <- createSampleProposal 1 0 owner1 dao

  advanceTime (sec 10)
  withSender (AddressResolved admin) $ call dao (Call @"Set_voting_period") 20

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  withSender (AddressResolved owner2) $ do
    call dao (Call @"Vote") [params]
    advanceTime (sec 25)
    call dao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER dao
