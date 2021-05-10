-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import Time (sec)

import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario, nettestScenarioOnEmulator, nettestScenarioOnEmulatorCaps, nettestScenarioOnNetworkCaps)
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
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

downvote :: ProposalKey -> PermitProtected VoteParam
downvote = vote False

data FailureReason = QuorumNotMet | Downvoted

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "Proposal creator:"
      [ nettestScenarioOnEmulatorCaps "BaseDAO - can propose a valid proposal (emulator)" $
          validProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) getTotalSupplyEmulator

      , nettestScenarioOnEmulator "cannot propose an invalid proposal (rejected)" $
          \_emulated ->
            uncapsNettest $ rejectProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "cannot propose a non-unique proposal" $
          \_emulated ->
            uncapsNettest $ nonUniqueProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "cannot propose in a non-proposal period" $
          \_emulated ->
            uncapsNettest $ nonProposalPeriodProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Voter:"
      [ nettestScenarioOnEmulator "can vote on a valid proposal" $
          \_emulated ->
            uncapsNettest $ voteValidProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "cannot vote non-existing proposal" $
          \_emulated ->
            uncapsNettest $ voteNonExistingProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can vote on multiple proposals" $
          \_emulated ->
            uncapsNettest $ voteMultiProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated ->
            uncapsNettest $ voteOutdatedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]


  , nettestScenarioOnEmulator "cannot vote if the vote amounts exceeds token balance" $
      \_emulated ->
        uncapsNettest $ insufficientTokenVote (originateLigoDaoWithConfigDesc dynRecUnsafe)

  -- Note: When checking storage, we need to split the test into 2 (emulator and network) as demonstrated below:
  , nettestScenarioOnEmulatorCaps "cannot propose with insufficient tokens (emulator) " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDate . fsStorage) <$> getFullStorage addr)
  , nettestScenarioOnNetworkCaps "cannot propose with insufficient tokens (network) " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDate . fsStorage) <$> getFullStorageView addr)

  , testGroup "Permit:"
      [ nettestScenarioOnEmulator "can vote from another user behalf" $
          \_emulated ->
            uncapsNettest $ voteWithPermit (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulatorCaps "counter works properly in permits" $
            voteWithPermitNonce (originateLigoDaoWithConfigDesc dynRecUnsafe) getVotePermitsCounterEmulator
      ]
  , testGroup "Admin:"
      [ nettestScenarioOnEmulatorCaps "can flush proposals that got accepted" $
          flushAcceptedProposals (originateLigoDaoWithConfigDesc dynRecUnsafe) getTotalSupplyEmulator
      , nettestScenarioOnEmulator "can flush 2 proposals that got accepted" $
          \_emulated ->
            uncapsNettest $ flushAcceptedProposalsWithAnAmount (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to not meeting quorum_threshold" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to negative votes" $
          \_emulated ->
            uncapsNettest $ flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "flush should not affecting ongoing proposals" $
          \_emulated ->
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
      ]

  , testGroup "Bounded Value"
      [ nettestScenarioOnEmulator "bounded value on proposals" $
          \_emulated ->
            uncapsNettest $ proposalBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioOnEmulator "bounded value on votes" $
          \_emulated ->
            uncapsNettest $ votesBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

  , testGroup "Freeze-Unfreeze"
      [ nettestScenario "can freeze tokens" $
          uncapsNettest $ freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulator "cannot unfreeze tokens from the same period" $
          \_emulated ->
            uncapsNettest $ cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulator "can unfreeze tokens from the previous period" $
          \_emulated ->
            uncapsNettest $ canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

 , testGroup "LIGO-specific proposal tests:"
    [ nettestScenarioOnEmulatorCaps "can propose a valid proposal with a fixed fee" $ do
        ((proposer, _), _, dao, _, _) <-
          originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 42))
        let params = ProposeParams
              { ppFrozenToken = 10
              , ppProposalMetadata = lPackValueRaw @Integer 1
              }

        withSender (AddressResolved proposer) $
          call dao (Call @"Freeze") (#amount .! 52)
        advanceTime (sec 20)

        withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
        checkTokenBalance frozenTokenId dao proposer 152

        totalSupply <- getTotalSupplyEmulator (AddressResolved $ unTAddress dao) frozenTokenId
        totalSupply @== 252 -- initial = 0

    , nettestScenarioOnEmulator "cannot propose with insufficient tokens to pay the fee" $
        \_emulated -> uncapsNettest $ do
          ((proposer, _), _, dao, _, _) <-
            originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 100))

          withSender (AddressResolved proposer) $
            call dao (Call @"Freeze") (#amount .! 52)
          advanceTime (sec 10)

          let params = ProposeParams
                { ppFrozenToken = 1
                , ppProposalMetadata = lPackValueRaw @Integer 1
                }
          withSender (AddressResolved proposer) $ call dao (Call @"Propose") params
            & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

    , nettestScenarioOnEmulator "a proposer is returned a fee after the proposal succeeds" $
        \_emulated -> uncapsNettest $ do
          ((proposer, _), (voter, _), dao, _, admin) <-
            originateLigoDaoWithConfigDesc dynRecUnsafe ((ConfigDesc $ VotingPeriod 60) >>- (ConfigDesc (FixedFee 42)))

          withSender (AddressResolved voter) $
            call dao (Call @"Freeze") (#amount .! 20)

          withSender (AddressResolved proposer) $
            call dao (Call @"Freeze") (#amount .! 42)

          advanceTime (sec 61)
          key1 <- createSampleProposal 1 60 proposer dao
          advanceTime (sec 60)
          let vote_ =
                NoPermit VoteParam
                  { vVoteType = True
                  , vVoteAmount = 10
                  , vProposalKey = key1
                  }
          withSender (AddressResolved voter) $
            call dao (Call @"Vote") [vote_]

          let expectedFrozen = 100 + 42 + 10 -- 'createSampleProposal' freezes 10 tokens
          checkTokenBalance frozenTokenId dao proposer expectedFrozen

          advanceTime (sec 60)
          withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

          checkTokenBalance frozenTokenId dao proposer 152

    , nettestScenarioOnEmulator "the fee is burned if the proposal fails" $
        \_emulated -> uncapsNettest $ burnsFeeOnFailure Downvoted

    , nettestScenarioOnEmulator "the fee is burned if the proposal doesn't meet the quorum" $
        \_emulated -> uncapsNettest $ burnsFeeOnFailure QuorumNotMet
    ]
  ]

nonProposalPeriodProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonProposalPeriodProposal originateFn = do
  ((owner1, _), _, dao, _, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10)

  advanceTime (sec 10)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorNoArg #nOT_PROPOSING_PERIOD dao

freezeTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
freezeTokens originateFn = do
  ((owner1, _), _, dao, tokenContract, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 110
  -- Check that the FA2 token got a transfer call as expected.
  checkStorage (AddressResolved $ unTAddress tokenContract)
    (toVal [[FA2.TransferItem
      { tiFrom = owner1
      , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])

burnsFeeOnFailure
  :: forall caps base m. (MonadNettest caps base m)
  => FailureReason -> m ()
burnsFeeOnFailure reason = do
  ((proposer, _), (voter, _), dao, _, admin) <-
      originateLigoDaoWithConfigDesc dynRecUnsafe ((ConfigDesc $ VotingPeriod 60) >>- (ConfigDesc $ FixedFee 42))

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

  let expectedFrozen = 100 + 42 + 10 -- 'createSampleProposal' freezes 10 tokens
  checkTokenBalance frozenTokenId dao proposer expectedFrozen

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

  -- Tokens frozen with the proposal are returned as unstaked (but still
  -- frozen), except for the fee and slash amount. The latter is zero in this
  -- case, so we expect 42 tokens to be burnt
  let expectedBurn = 42
  checkTokenBalance frozenTokenId dao proposer (100 + (52 - expectedBurn))

cannotUnfreezeFromSamePeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
cannotUnfreezeFromSamePeriod originateFn = do
  ((owner1, _), _, dao, _, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 110

  -- Cannot unfreeze in the same period
  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

canUnfreezeFromPreviousPeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
canUnfreezeFromPreviousPeriod originateFn = do
  ((owner1, _), _, dao, tokenContract, _) <- originateFn testConfig

  withSender (AddressResolved owner1) $ call dao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 110

  advanceTime (sec 15)

  withSender (AddressResolved owner1) $ call dao (Call @"Unfreeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dao owner1 100
  -- Check that the FA2 token got a transfer call as expected.
  checkStorage (AddressResolved $ unTAddress tokenContract)
    (toVal
      [ [ FA2.TransferItem
        { tiFrom = unTAddress dao
        , tiTxs = [FA2.TransferDestination { tdTo = owner1, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
        }]
      , [FA2.TransferItem
        { tiFrom = owner1
        , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])

insufficientTokenProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> (AddressOrAlias -> m Int) -> m ()
insufficientTokenProposal originateFn getProposalAmountFn = do
  ((owner1, _), _, dao, _, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao
  amt <- getProposalAmountFn (AddressResolved $ unTAddress dao)
  amt @== 0

insufficientTokenVote
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
insufficientTokenVote originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn voteConfig
  advanceTime (sec 10)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 100)

  -- Create sample proposal
  key1 <- createSampleProposal 1 10 owner1 dao
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
  advanceTime (sec 10)

  withSender (AddressResolved owner2) $ call dao (Call @"Vote") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dao

voteWithPermit
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteWithPermit originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn voteConfig
  advanceTime (sec 10)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 2)

  -- Create sample proposal
  key1 <- createSampleProposal 1 10 owner1 dao

  params <- permitProtect (AddressResolved owner1) =<< addDataToSign dao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 10)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dao owner1 112

voteWithPermitNonce
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetVotePermitsCounterFn m -> m ()
voteWithPermitNonce originateFn getVotePermitsCounterFn = do

  ((owner1, _), (owner2, _), dao, _, _) <- originateFn voteConfig

  advanceTime (sec 10)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 50)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 50)

  -- Create sample proposal
  key1 <- createSampleProposal 1 10 owner1 dao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 10)
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
  (Nonce counter) <- getVotePermitsCounterFn (AddressResolved $ unTAddress dao)
  counter @== 2

flushNotAffectOngoingProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushNotAffectOngoingProposals originateFn = do
  ((owner1, _), _, dao, _, admin) <-
    originateFn (testConfig >>- (ConfigDesc $ VotingPeriod (2 *60)))

  advanceTime (sec 120)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 20)

  advanceTime (sec 125)

  _key1 <- createSampleProposal 1 0 owner1 dao
  _key2 <- createSampleProposal 2 0 owner1 dao
  advanceTime (sec 125)
  withSender (AddressResolved admin) $
    call dao (Call @"Flush") $ Flush_amount 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  -- checkIfAProposalExist (key2 :: ByteString) dao

flushAcceptedProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetTotalSupplyFn m -> m ()
flushAcceptedProposals originateFn getTotalSupplyFn = do
-- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  ((owner1, _), (owner2, _), dao, _, admin) <-
    originateFn (testConfig >>- (ConfigDesc $ VotingPeriod 60))

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
  checkTokenBalance (frozenTokenId) dao owner1 110
  checkTokenBalance (frozenTokenId) dao owner2 103

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance (frozenTokenId) dao owner1 110

  checkTokenBalance (frozenTokenId) dao owner2 103

  totalSupply <- getTotalSupplyFn (AddressResolved $ unTAddress dao) frozenTokenId
  totalSupply @== 213 -- initial = 0

flushAcceptedProposalsWithAnAmount
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushAcceptedProposalsWithAnAmount originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateFn testConfig

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

  checkTokenBalance frozenTokenId dao owner1 140

  advanceTime (sec 10)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 2

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
  checkTokenBalance frozenTokenId dao owner1 140

flushRejectProposalQuorum
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushRejectProposalQuorum originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin)
    <- originateFn (configWithRejectedProposal
        >>- (ConfigDesc (QuorumThreshold 3 5))
        >>- (ConfigDesc $ VotingPeriod 60))

  advanceTime (sec 60)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 5)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 65 owner1 dao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 3
          , vProposalKey = key1
          }
        ]
  advanceTime (sec 60)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") votes

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance frozenTokenId dao owner1 105
  checkTokenBalance frozenTokenId dao owner2 105 -- Since voter tokens are not burned

flushRejectProposalNegativeVotes
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushRejectProposalNegativeVotes originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin)
    <- originateFn (configWithRejectedProposal >>- (ConfigDesc (QuorumThreshold 3 100)) >>- (ConfigDesc (VotingPeriod 60)))

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
  checkTokenBalance frozenTokenId dao owner1 110

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance frozenTokenId dao owner1 105
  checkTokenBalance frozenTokenId dao owner2 103

flushWithBadConfig
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushWithBadConfig originateFn = do
  ((owner1, _), (owner2, _), dao, _, admin) <-
    originateFn (badRejectedValueConfig >>- (ConfigDesc (QuorumThreshold 1 2)) >>- (ConfigDesc (VotingPeriod 60)))

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

  advanceTime (sec 61)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

  checkTokenBalance frozenTokenId dao owner1 100
  checkTokenBalance frozenTokenId dao owner2 103

flushDecisionLambda
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushDecisionLambda originateFn = do
  consumer <- originateSimple "consumer" [] (contractConsumer)
  ((owner1, _), (owner2, _), dao, _, admin) <-
    originateFn ((decisionLambdaConfig consumer) >>- (ConfigDesc $ VotingPeriod 60))

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 10)
  advanceTime (sec 65)
  key1 <- createSampleProposal 1 60 owner1 dao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 10
        , vProposalKey = key1
        }
  advanceTime (sec 60)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote']

  advanceTime (sec 60)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") $ Flush_amount 100

  results <- fromVal <$> getStorage (AddressResolved $ toAddress consumer)
  assert (results == (#proposer <.!> [owner1]))
    "Unexpected accepted proposals list"

proposalBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
proposalBoundedValue originateFn = do
  ((owner1, _), _, dao, _, _) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts{ cmMaxProposals = Just 1 }
    )
  advanceTime (sec 11)

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 20)

  advanceTime (sec 10)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender (AddressResolved owner1) $ do
    call dao (Call @"Propose") params
    call dao (Call @"Propose") params
      & expectCustomErrorNoArg #mAX_PROPOSALS_REACHED dao

votesBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
votesBoundedValue originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn
    ( voteConfig >>-
      ConfigDesc configConsts{ cmMaxVotes = Just 1 }
    )
  advanceTime (sec 10)
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 2)

  key1 <- createSampleProposal 1 10 owner2 dao
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
  advanceTime (sec 10)
  withSender (AddressResolved owner1) $ do
    call dao (Call @"Vote") [downvote']
    call dao (Call @"Vote") [upvote']
      & expectCustomErrorNoArg #mAX_VOTES_REACHED dao
