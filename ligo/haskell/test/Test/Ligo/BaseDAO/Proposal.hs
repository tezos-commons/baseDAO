-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum
import Lorentz hiding (assert, (>>))

import Test.Tasty (TestTree, testGroup)
import Time (sec)

import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario, nettestScenarioOnEmulator)
import Util.Named

import Ligo.BaseDAO.Types
import Ligo.BaseDAO.ShareTest.Common hiding (createSampleProposal)
import Ligo.BaseDAO.ConfigDesc
import Test.Ligo.BaseDAO.Common

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
  [ testGroup "Proposal creator:"
      [ nettestScenario "BaseDAO - can propose a valid proposal" $
          uncapsNettest $ validProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot propose an invalid proposal (rejected)" $
          uncapsNettest $ rejectProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot propose a non-unique proposal" $
          uncapsNettest $ nonUniqueProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot propose in a non-proposal period" $
          uncapsNettest $ nonProposalPeriodProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Voter:"
      [ nettestScenario "can vote on a valid proposal" $
          uncapsNettest $ voteValidProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "cannot vote non-existing proposal" $
          uncapsNettest $ voteNonExistingProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "can vote on multiple proposals" $
          uncapsNettest $ voteMultiProposals True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated ->
            uncapsNettest $ voteOutdatedProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]


  , nettestScenario "cannot vote if the vote amounts exceeds token balance" $
      uncapsNettest $ insufficientTokenVote True (originateLigoDaoWithConfigDesc dynRecUnsafe)

  , nettestScenario "cannot propose with insufficient tokens" $
      uncapsNettest $ insufficientTokenProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)

  , testGroup "Permit:"
      [ nettestScenario "can vote from another user behalf" $
          uncapsNettest $ voteWithPermit True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "counter works properly in permits" $
          uncapsNettest $ voteWithPermitNonce True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]
  , testGroup "Admin:"
      [ nettestScenario "can set voting period"  $
          uncapsNettest $ setVotingPeriod True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "can set quorum threshold" $
          uncapsNettest $ setQuorumThreshold True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can flush proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposals True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush 2 proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposalsWithAnAmount True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to not meeting quorum_threshold" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalQuorum True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to negative votes" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalNegativeVotes True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "flush should not affecting ongoing proposals" $
          uncapsNettest $ flushNotAffectOngoingProposals True
            (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "flush with bad cRejectedProposalReturnValue" $
          \_emulated ->
            uncapsNettest $ flushWithBadConfig True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      -- TODO [#15]: admin burn proposer token and test "flush"

      -- TODO [#38]: Improve this when contract size is smaller
      , nettestScenarioOnEmulator "flush and run decision lambda" $
          \_emulated ->
            uncapsNettest $ flushDecisionLambda True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can drop proposals" $
          \_emulated ->
            uncapsNettest $ dropProposal True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Bounded Value"
      [ nettestScenario "bounded value on proposals" $
          uncapsNettest $ proposalBoundedValue True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "bounded value on votes" $
          uncapsNettest $ votesBoundedValue True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "bounded range on quorum_threshold" $
          uncapsNettest $ quorumThresholdBound True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenario "bounded range on voting_period" $
          uncapsNettest $ votingPeriodBound True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Freeze-Unfreeze"
      [ nettestScenario "can freeze tokens" $
          uncapsNettest $ freezeTokens True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "cannot unfreeze tokens from the same period" $
          uncapsNettest $ cannotUnfreezeFromSamePeriod True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "can unfreeze tokens from the previous period" $
          uncapsNettest $ canUnfreezeFromPreviousPeriod True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "handle voting period change" $
          uncapsNettest $ canHandleVotingPeriodChange True (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenario "handle voting period change" $
          uncapsNettest $ votingPeriodChange True (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

 , testGroup "LIGO-specific proposal tests:"
    [ nettestScenario "can propose a valid proposal with a fixed fee" $
        uncapsNettest $ do
          ((proposer, _), _, dao, admin) <- originateLigoDao
          withSender (AddressResolved admin) $
            call dao (Call @"Set_fixed_fee_in_token") 42
          let params :: ProposeParamsL = ProposeParams
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
              & expectError (VoidResult (148 :: Natural)) -- initial = 200

          withSender (AddressResolved proposer) $
            call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
              & expectError (VoidResult (52 :: Natural)) -- initial = 0

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
            & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

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
            & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

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
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
nonProposalPeriodProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10)

  advanceTime (sec 10)

  let params :: ProposeParamsL = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorNoArg #nOT_PROPOSING_PERIOD

freezeTokens
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
freezeTokens _ originateFn = do
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
    call dao (Call @"Set_quorum_threshold") 1
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
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
cannotUnfreezeFromSamePeriod _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 10

  -- Cannot unfreeze in the same period
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

canUnfreezeFromPreviousPeriod
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
canUnfreezeFromPreviousPeriod _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 10

  advanceTime (sec 15)

  -- Cannot unfreeze in the same period
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 0
  checkTokenBalance unfrozenTokenId dao owner1 100

canHandleVotingPeriodChange
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
canHandleVotingPeriodChange _ originateFn = do
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
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

  advanceTime (sec 5)
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)

insufficientTokenProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
insufficientTokenProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

insufficientTokenVote
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
insufficientTokenVote _ originateFn = do
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
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

voteWithPermit
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
voteWithPermit _ originateFn = do
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
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
voteWithPermitNonce _ originateFn = do

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
      & expectCustomError #mISSIGNED (checkedCoerce $ lPackValue dataToSign2)

    -- Nonce from future
    call dao (Call @"Vote") [params3]
      & expectCustomError #mISSIGNED (checkedCoerce $ lPackValue dataToSign2)

    -- Good nonce after the previous successful entrypoint call
    call dao (Call @"Vote") [params2]

  -- Check counter
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_vote_permit_counter") (mkVoid ())
      & expectError (VoidResult (2 :: Natural))

flushNotAffectOngoingProposals
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushNotAffectOngoingProposals _ originateFn = do
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
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushAcceptedProposals _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig

  -- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") 1

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
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 10
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- proposer

  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97 -- voter

  -- Check total supply (After flush, no changes are made.)
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
      & expectError (VoidResult (187 :: Natural)) -- initial = 200

  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError (VoidResult (13 :: Natural)) -- initial = 0


flushAcceptedProposalsWithAnAmount
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushAcceptedProposalsWithAnAmount _ originateFn = do
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
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER
    call dao (Call @"Vote") [vote' key2]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER

    -- Proposal is over but not affected
    call dao (Call @"Vote") [vote' key3]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER

    -- Proposal is not yet over
    call dao (Call @"Vote") [vote' key4]

  -- Only 2 proposals are flush, so only 20 tokens are unfrozen back.
  checkTokenBalance (frozenTokenId) dao owner1 40
  checkTokenBalance (unfrozenTokenId) dao owner1 60

flushRejectProposalQuorum
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushRejectProposalQuorum _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn configWithRejectedProposal

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") 3
  advanceTime (sec 60)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2)

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
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 5
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (frozenTokenId) dao owner2 2
  checkTokenBalance (unfrozenTokenId) dao owner2 98 -- voter

flushRejectProposalNegativeVotes
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushRejectProposalNegativeVotes _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn configWithRejectedProposal

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") 3

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
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 5
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97 -- voter

flushWithBadConfig
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushWithBadConfig _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn badRejectedValueConfig

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") 2

  advanceTime (sec 60)
  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 1)
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
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- slash all frozen values
  checkTokenBalance (frozenTokenId) dao owner2 1
  checkTokenBalance (unfrozenTokenId) dao owner2 99

flushDecisionLambda
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
flushDecisionLambda _ originateFn = do
  consumer <- originateSimple "consumer" [] (contractConsumer)
  ((owner1, _), (owner2, _), dao, admin) <- originateFn (decisionLambdaConfig consumer)

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 60
    call dao (Call @"Set_quorum_threshold") 1

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
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
dropProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn badRejectedValueConfig

  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_voting_period") 20
    call dao (Call @"Set_quorum_threshold") 2
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
      & expectCustomErrorNoArg #fAIL_DROP_PROPOSAL_NOT_ACCEPTED
    call dao (Call @"Drop_proposal") key3
      & expectCustomErrorNoArg #fAIL_DROP_PROPOSAL_NOT_OVER

  -- 30 tokens are frozen in total, but 10 tokens are returned after drop_proposal
  checkTokenBalance (frozenTokenId) dao owner1 30
  checkTokenBalance (unfrozenTokenId) dao owner1 70

setVotingPeriod
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
setVotingPeriod _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  let param = 60 * 60 -- 1 hour

  withSender (AddressResolved owner1) $
    call dao (Call @"Set_voting_period") param
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $
    call dao (Call @"Set_voting_period") param
  -- TODO [#31]: checkStorage
  --

setQuorumThreshold
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
setQuorumThreshold _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  let param = 100

  withSender (AddressResolved owner1) $
    call dao (Call @"Set_quorum_threshold") param
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $
    call dao (Call @"Set_quorum_threshold") param
  -- TODO [#31]: checkStorage

proposalBoundedValue
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
proposalBoundedValue _ originateFn = do
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
      & expectCustomErrorNoArg #mAX_PROPOSALS_REACHED

votesBoundedValue
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
votesBoundedValue _ originateFn = do
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
      & expectCustomErrorNoArg #mAX_VOTES_REACHED

quorumThresholdBound
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
quorumThresholdBound _ originateFn = do
  (_, _, dao, admin) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts
        { cmMinQuorumThreshold = Just 1
        , cmMaxQuorumThreshold = Just 2
        }
    )
  withSender (AddressResolved admin) $ do
    call dao (Call @"Set_quorum_threshold") 1
    call dao (Call @"Set_quorum_threshold") 2
    call dao (Call @"Set_quorum_threshold") 0
      & expectCustomErrorNoArg #oUT_OF_BOUND_QUORUM_THRESHOLD
    call dao (Call @"Set_quorum_threshold") 3
      & expectCustomErrorNoArg #oUT_OF_BOUND_QUORUM_THRESHOLD

votingPeriodBound
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
votingPeriodBound _ originateFn = do
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
      & expectCustomErrorNoArg #oUT_OF_BOUND_VOTING_PERIOD
    call dao (Call @"Set_voting_period") 3
      & expectCustomErrorNoArg #oUT_OF_BOUND_VOTING_PERIOD

votingPeriodChange
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
votingPeriodChange _ originateFn = do
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
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER

validProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
validProposal  _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  advanceTime (sec 10)
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10)
  advanceTime (sec 10)

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  checkTokenBalance frozenTokenId dao owner1 10
  checkTokenBalance unfrozenTokenId dao owner1 90

  -- Check total supply
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
      & expectError (VoidResult (190 :: Natural)) -- initial = 200

  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError (VoidResult (10 :: Natural)) -- initial = 0

rejectProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
rejectProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  advanceTime (sec 10)
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10)
  advanceTime (sec 10)

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK

nonUniqueProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
nonUniqueProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  advanceTime (sec 10)
  _ <- createSampleProposal 1 10 owner1 dao
  createSampleProposal 1 10 owner1 dao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE

voteValidProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
voteValidProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig
  advanceTime (sec 120)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2)

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 120 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 120)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
  checkTokenBalance (unfrozenTokenId) dao owner2 98
  checkTokenBalance (frozenTokenId) dao owner2 2
  -- TODO [#31]: check if the vote is updated properly

voteNonExistingProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
voteNonExistingProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn testConfig
  advanceTime (sec 10)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2)

  -- Create sample proposal
  _ <- createSampleProposal 1 15 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = HashUnsafe "\11\12\13"
        }

  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
    & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

voteMultiProposals
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
voteMultiProposals _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  advanceTime (sec 120)
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 20)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 5)
  advanceTime (sec 120)

  -- Create sample proposal
  key1 <- createSampleProposal 1 0 owner1 dao
  key2 <- createSampleProposal 2 0 owner1 dao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 2
            , vProposalKey = key1
            }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            }
        ]

  advanceTime (sec 120)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") params
  checkTokenBalance (unfrozenTokenId) dao owner2 95
  checkTokenBalance (frozenTokenId) dao owner2 5
  -- TODO [#31]: check storage if the vote update the proposal properly

voteOutdatedProposal
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc ConfigL -> OriginateFn ParameterL m) -> m ()
voteOutdatedProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn testConfig
  advanceTime (sec 10)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2)
  -- Create sample proposal
  key1 <- createSampleProposal 1 10 owner1 dao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 10)
  withSender (AddressResolved owner2) $ do
    call dao (Call @"Vote") [params]
    advanceTime (sec 25)
    call dao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER
