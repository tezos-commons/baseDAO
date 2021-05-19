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
import Morley.Nettest.Tasty (nettestScenario, nettestScenarioOnEmulatorCaps, nettestScenarioOnNetworkCaps)
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

      , nettestScenarioOnEmulatorCaps "cannot propose an invalid proposal (rejected)" $
          rejectProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot propose a non-unique proposal" $
          nonUniqueProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot propose in a non-proposal period" $
          nonProposalPeriodProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Voter:"
      [ nettestScenarioOnEmulatorCaps "can vote on a valid proposal" $
          voteValidProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot vote non-existing proposal" $
          voteNonExistingProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "can vote on multiple proposals" $
          voteMultiProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot vote on outdated proposal" $
          voteOutdatedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]


  , nettestScenarioOnEmulatorCaps "cannot vote if the vote amounts exceeds token balance" $
      insufficientTokenVote (originateLigoDaoWithConfigDesc dynRecUnsafe)

  -- Note: When checking storage, we need to split the test into 2 (emulator and network) as demonstrated below:
  , nettestScenarioOnEmulatorCaps "cannot propose with insufficient tokens (emulator) " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDate . fsStorage) <$> getFullStorage addr)

  , nettestScenarioOnNetworkCaps "cannot propose with insufficient tokens (network) " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDate . fsStorage) <$> getFullStorageView addr)

  , testGroup "Permit:"
      [ nettestScenarioOnEmulatorCaps "can vote from another user behalf" $
          voteWithPermit (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "counter works properly in permits" $
          voteWithPermitNonce (originateLigoDaoWithConfigDesc dynRecUnsafe) getVotePermitsCounterEmulator

      ]
  , testGroup "Admin:"
      [ nettestScenarioOnEmulatorCaps "can flush proposals that got accepted" $
          flushAcceptedProposals (originateLigoDaoWithConfigDesc dynRecUnsafe) getTotalSupplyEmulator

      , nettestScenarioOnEmulatorCaps "can flush 2 proposals that got accepted" $
          flushAcceptedProposalsWithAnAmount (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "can flush proposals that got rejected due to not meeting quorum_threshold" $
          flushRejectProposalQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "can flush proposals that got rejected due to negative votes" $
          flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "flush should not affect proposals that cannot be flushed yet" $
          flushProposalFlushTimeNotReach (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "flush should fail on expired proposals" $
          flushFailOnExpiredProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "flush with bad cRejectedProposalReturnValue" $
          flushWithBadConfig (originateLigoDaoWithConfigDesc dynRecUnsafe)

      -- TODO [#15]: dodAdmin burn proposer token and test "flush"

      -- TODO [#38]: Improve this when contract size is smaller
      , nettestScenarioOnEmulatorCaps "flush and run decision lambda" $
          flushDecisionLambda (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "can drop proposals, only when allowed" $
          dropProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Bounded Value"
      [ nettestScenarioOnEmulatorCaps "bounded value on proposals" $
          proposalBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "bounded value on votes" $
          votesBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Freeze-Unfreeze"
      [ nettestScenario "can freeze tokens" $
          uncapsNettest $ freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot unfreeze tokens from the same period" $
          cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)


      , nettestScenarioOnEmulatorCaps "can unfreeze tokens from the previous period" $
          canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

 , testGroup "LIGO-specific proposal tests:"
    [ nettestScenarioOnEmulatorCaps "can propose a valid proposal with a fixed fee" $ do
        DaoOriginateData{..} <-
          originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 42))
        let params = ProposeParams
              { ppFrozenToken = 10
              , ppProposalMetadata = lPackValueRaw @Integer 1
              }
        let proposer = dodOwner1

        withSender proposer $
          call dodDao (Call @"Freeze") (#amount .! 52)
        -- Advance one voting period to a proposing stage.
        advanceTime (sec 10)

        withSender proposer $ call dodDao (Call @"Propose") params
        checkTokenBalance frozenTokenId dodDao proposer 152

        totalSupply <- getTotalSupplyEmulator (unTAddress dodDao) frozenTokenId
        totalSupply @== 252 -- initial = 0

    , nettestScenarioOnEmulatorCaps "cannot propose with insufficient tokens to pay the fee" $ do
        DaoOriginateData{..} <-
          originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 100))
        let proposer = dodOwner1

        withSender proposer $
          call dodDao (Call @"Freeze") (#amount .! 52)
        -- Advance one voting period to a proposing stage.
        advanceTime (sec 10)

        let params = ProposeParams
              { ppFrozenToken = 1
              , ppProposalMetadata = lPackValueRaw @Integer 1
              }
        withSender proposer $ call dodDao (Call @"Propose") params
          & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao

    , nettestScenarioOnEmulatorCaps "a proposer is returned a fee after the proposal succeeds" $ do
          DaoOriginateData{..} <-
            originateLigoDaoWithConfigDesc dynRecUnsafe
              (   (ConfigDesc $ VotingPeriod 60)
              >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
              >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
              >>- (ConfigDesc (FixedFee 42))
              )
          let proposer = dodOwner1
          let voter = dodOwner2

          withSender voter $
            call dodDao (Call @"Freeze") (#amount .! 20)

          withSender proposer $
            call dodDao (Call @"Freeze") (#amount .! 42)

          withSender proposer $
            call dodDao (Call @"Freeze") (#amount .! 10)

          -- Advance one voting period to a proposing stage.
          advanceTime (sec 60)
          key1 <- createSampleProposal 1 proposer dodDao
          -- Advance one voting period to a voting stage.
          advanceTime (sec 60)
          let vote_ =
                NoPermit VoteParam
                  { vVoteType = True
                  , vVoteAmount = 10
                  , vProposalKey = key1
                  }
          withSender voter $
            call dodDao (Call @"Vote") [vote_]

          let expectedFrozen = 100 + 42 + 10
          checkTokenBalance frozenTokenId dodDao proposer expectedFrozen

          -- Advance one voting period to a proposing stage.
          advanceTime (sec 60)
          withSender dodAdmin $ call dodDao (Call @"Flush") 100

          checkTokenBalance frozenTokenId dodDao proposer 152

    , nettestScenarioOnEmulatorCaps "a proposal is rejected if upvotes > downvotes and quorum threshold is not met" $ do
          DaoOriginateData{..} <-
            originateLigoDaoWithConfigDesc dynRecUnsafe
              ((  ConfigDesc $ mkQuorumThreshold 1 20)
              >>- (ConfigDesc $ VotingPeriod 60)
              >>- (ConfigDesc (FixedFee 42))
              >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 1800 })
              )
          let proposer = dodOwner1
          let voter = dodOwner2
          let dao = dodDao
          let admin = dodAdmin

          withSender voter $
            call dao (Call @"Freeze") (#amount .! 28)

          withSender proposer $
            call dao (Call @"Freeze") (#amount .! 42)

          withSender proposer $
            call dao (Call @"Freeze") (#amount .! 10)

          -- Advance one voting period to a proposing stage.
          advanceTime (sec 60)
          key1 <- createSampleProposal 1 proposer dao
          -- Advance one voting period to a voting stage.
          advanceTime (sec 60)
          let vote_ =
                NoPermit VoteParam
                  { vVoteType = True
                  , vVoteAmount = 13
                    -- The minimum votes that is required to pass is 280 * 1/20  = 14.
                  , vProposalKey = key1
                  }
          withSender voter $
            call dao (Call @"Vote") [vote_]

          let expectedFrozen = 100 + 42 + 10
          checkTokenBalance frozenTokenId dao proposer expectedFrozen

          -- Advance one voting period to a proposing stage.
          advanceTime (sec 60)
          withSender admin $ call dao (Call @"Flush") 100

          checkTokenBalance frozenTokenId dao proposer 110 -- We expect 42 tokens to have burned

    , nettestScenarioOnEmulatorCaps "a proposal succeeds if upvotes > downvotes and quorum threshold is met" $ do
          DaoOriginateData{..} <-
            originateLigoDaoWithConfigDesc dynRecUnsafe
              ((  ConfigDesc $ mkQuorumThreshold 1 20)
              >>- (ConfigDesc $ VotingPeriod 60)
              >>- (ConfigDesc (FixedFee 42))
              >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 1800 })
              )
          let proposer = dodOwner1
          let voter = dodOwner2
          let dao = dodDao
          let admin = dodAdmin

          withSender voter $
            call dao (Call @"Freeze") (#amount .! 28)

          withSender proposer $
            call dao (Call @"Freeze") (#amount .! 42)

          withSender proposer $
            call dao (Call @"Freeze") (#amount .! 10)

          -- Advance one voting period to a proposing stage.
          advanceTime (sec 60)
          key1 <- createSampleProposal 1 proposer dao
          -- Advance one voting period to a voting stage.
          advanceTime (sec 60)
          let vote_ =
                NoPermit VoteParam
                  { vVoteType = True
                  , vVoteAmount = 14
                    -- The minimum votes that is required to pass is 280 * 1/20  = 14.
                  , vProposalKey = key1
                  }
          withSender voter $
            call dao (Call @"Vote") [vote_]

          let expectedFrozen = 100 + 42 + 10
          checkTokenBalance frozenTokenId dao proposer expectedFrozen

          -- Advance one voting period to a proposing stage.
          advanceTime (sec 60)
          withSender admin $ call dao (Call @"Flush") 100

          checkTokenBalance frozenTokenId dao proposer 152

    , nettestScenarioOnEmulatorCaps "the fee is burned if the proposal fails" $
        burnsFeeOnFailure Downvoted
    , nettestScenarioOnEmulatorCaps "the fee is burned if the proposal doesn't meet the quorum" $
        burnsFeeOnFailure QuorumNotMet
    ]
  ]

nonProposalPeriodProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonProposalPeriodProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance two voting periods to another voting stage.
  advanceTime (sec 20)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomErrorNoArg #nOT_PROPOSING_PERIOD dodDao

freezeTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
freezeTokens originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 110
  -- Check that the FA2 token got a transfer call as expected.
  checkStorage (unTAddress dodTokenContract)
    (toVal [[FA2.TransferItem
      { tiFrom = dodOwner1
      , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])

burnsFeeOnFailure
  :: forall caps base m. (MonadNettest caps base m)
  => FailureReason -> m ()
burnsFeeOnFailure reason = do
  DaoOriginateData{..} <-
      originateLigoDaoWithConfigDesc dynRecUnsafe
        (   (ConfigDesc $ VotingPeriod 60)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
        >>- (ConfigDesc $ FixedFee 42)
        )
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 42)

  withSender voter $
    call dodDao (Call @"Freeze") (#amount .! 1)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 61)
  key1 <- createSampleProposal 1 proposer dodDao

  -- Advance one voting period to a voting stage.
  advanceTime (sec 60)
  case reason of
    Downvoted -> do
      withSender voter $
        call dodDao (Call @"Vote") [downvote key1]
    QuorumNotMet -> return ()

  let expectedFrozen = 100 + 42 + 10
  checkTokenBalance frozenTokenId dodDao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 61)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- Tokens frozen with the proposal are returned as unstaked (but still
  -- frozen), except for the fee and slash amount. The latter is zero in this
  -- case, so we expect 42 tokens to be burnt
  let expectedBurn = 42
  checkTokenBalance frozenTokenId dodDao proposer (100 + (52 - expectedBurn))

cannotUnfreezeFromSamePeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
cannotUnfreezeFromSamePeriod originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Cannot unfreeze in the same period
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao

canUnfreezeFromPreviousPeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
canUnfreezeFromPreviousPeriod originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 15)

  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 100
  -- Check that the FA2 token got a transfer call as expected.
  checkStorage (unTAddress dodTokenContract)
    (toVal
      [ [ FA2.TransferItem
        { tiFrom = unTAddress dodDao
        , tiTxs = [FA2.TransferDestination { tdTo = dodOwner1, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
        }]
      , [FA2.TransferItem
        { tiFrom = dodOwner1
        , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])

insufficientTokenProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> (Address -> m Int) -> m ()
insufficientTokenProposal originateFn getProposalAmountFn = do
  DaoOriginateData{..} <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao
  amt <- getProposalAmountFn (unTAddress dodDao)
  amt @== 0

insufficientTokenVote
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
insufficientTokenVote originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig
  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 100)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao
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
  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)

  withSender dodOwner2 $ call dodDao (Call @"Vote") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao

voteWithPermit
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteWithPermit originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 12)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  params <- permitProtect dodOwner1 =<< addDataToSign dodDao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)

  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dodDao dodOwner1 112

voteWithPermitNonce
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetVotePermitsCounterFn m -> m ()
voteWithPermitNonce originateFn getVotePermitsCounterFn = do

  DaoOriginateData{..} <- originateFn voteConfig

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 60)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 50)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)
  -- Going to try calls with different nonces
  signed1@(_          , _) <- addDataToSign dodDao (Nonce 0) voteParam
  signed2@(dataToSign2, _) <- addDataToSign dodDao (Nonce 1) voteParam
  signed3@(_          , _) <- addDataToSign dodDao (Nonce 2) voteParam

  params1 <- permitProtect dodOwner1 signed1
  params2 <- permitProtect dodOwner1 signed2
  params3 <- permitProtect dodOwner1 signed3

  withSender dodOwner2 $ do
    -- Good nonce
    call dodDao (Call @"Vote") [params1]

    -- Outdated nonce
    call dodDao (Call @"Vote") [params1]
      & expectCustomError #mISSIGNED dodDao (checkedCoerce $ lPackValue dataToSign2)

    -- Nonce from future
    call dodDao (Call @"Vote") [params3]
      & expectCustomError #mISSIGNED dodDao (checkedCoerce $ lPackValue dataToSign2)

    -- Good nonce after the previous successful entrypoint call
    call dodDao (Call @"Vote") [params2]

  -- Check counter
  (Nonce counter) <- getVotePermitsCounterFn (unTAddress dodDao)
  counter @== 2

flushProposalFlushTimeNotReach
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushProposalFlushTimeNotReach originateFn = do
  DaoOriginateData{..} <-
    originateFn (configWithRejectedProposal
        >>- (ConfigDesc $ VotingPeriod 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)

  _key1 <- createSampleProposal 1 dodOwner1 dodDao
  _key2 <- createSampleProposal 2 dodOwner1 dodDao
  -- Advance two voting period to another proposing stage.
  advanceTime (sec 20) -- skip voting period
  advanceTime (sec 21)
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  checkTokenBalance (frozenTokenId) dodDao dodOwner1 (100 + 5 + 5 + 10) -- first 2 proposals got flushed then slashed by 5, the last one is not affected.

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  -- checkIfAProposalExist (key2 :: ByteString) dodDao

flushAcceptedProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetTotalSupplyFn m -> m ()
flushAcceptedProposals originateFn getTotalSupplyFn = do
-- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  DaoOriginateData{..} <- originateFn (testConfig
      >>- (ConfigDesc $ VotingPeriod 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 60)

  -- Accepted Proposals
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  -- Advance one voting period to a voting stage.
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
  withSender dodOwner2 $
    call dodDao (Call @"Vote") [upvote', downvote']

  -- Checking balance of proposer and voters
  checkTokenBalance (frozenTokenId) dodDao dodOwner1 110
  checkTokenBalance (frozenTokenId) dodDao dodOwner2 103

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 61)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkTokenBalance (frozenTokenId) dodDao dodOwner1 110

  checkTokenBalance (frozenTokenId) dodDao dodOwner2 103

  totalSupply <- getTotalSupplyFn (unTAddress dodDao) frozenTokenId
  totalSupply @== 213 -- initial = 0

flushAcceptedProposalsWithAnAmount
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushAcceptedProposalsWithAnAmount originateFn = do
  DaoOriginateData{..}
    <- originateFn (configWithRejectedProposal
        >>- (ConfigDesc $ VotingPeriod 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        )

  -- [Voting]
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 6)

  advanceTime (sec 20)

  -- [Proposing]
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  key2 <- createSampleProposal 2 dodOwner1 dodDao
  advanceTime (sec 1)
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  let vote' key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 3
        , vProposalKey = key
        }

  advanceTime (sec 20)

  -- [Voting]
  withSender dodOwner2 $ do
    call dodDao (Call @"Vote") [vote' key1]
    call dodDao (Call @"Vote") [vote' key2]

  advanceTime (sec 22)

  -- [Proposing]
  withSender dodAdmin $ call dodDao (Call @"Flush") 2

  -- key1 and key2 are flushed. (Tokens remain the same, because they are all passed)
  checkTokenBalance frozenTokenId dodDao dodOwner1 130

  withSender dodAdmin $ call dodDao (Call @"Flush") 1

  -- key3 is rejected
  checkTokenBalance frozenTokenId dodDao dodOwner1 125


flushRejectProposalQuorum
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushRejectProposalQuorum originateFn = do
  DaoOriginateData{..}
    <- originateFn (configWithRejectedProposal
        >>- (ConfigDesc (mkQuorumThreshold 3 5))
        >>- (ConfigDesc $ VotingPeriod 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 5)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 3
          , vProposalKey = key1
          }
        ]
  -- Advance one voting period to a voting stage.
  advanceTime (sec 20)
  withSender dodOwner2 $ call dodDao (Call @"Vote") votes

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 21)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkTokenBalance frozenTokenId dodDao dodOwner1 105
  checkTokenBalance frozenTokenId dodDao dodOwner2 105 -- Since voter tokens are not burned

flushRejectProposalNegativeVotes
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushRejectProposalNegativeVotes originateFn = do
  DaoOriginateData{..}
    <- originateFn (configWithRejectedProposal
          >>- (ConfigDesc (mkQuorumThreshold 3 100))
          >>- (ConfigDesc (VotingPeriod 20))
          >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
          >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
          >>- (ConfigDesc (mkQuorumThreshold 3 100))
          )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

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
  -- Advance one voting period to a voting stage.
  advanceTime (sec 20)
  withSender dodOwner2 $ call dodDao (Call @"Vote") votes

  -- Check proposer balance
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 21)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkTokenBalance frozenTokenId dodDao dodOwner1 105
  checkTokenBalance frozenTokenId dodDao dodOwner2 103

flushWithBadConfig
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushWithBadConfig originateFn = do
  DaoOriginateData{..} <-
    originateFn (badRejectedValueConfig
      >>- (ConfigDesc (mkQuorumThreshold 1 2))
      >>- (ConfigDesc (VotingPeriod 20))
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      >>- (ConfigDesc (mkQuorumThreshold 1 2))
      )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  -- Advance one voting period to a voting stage.
  advanceTime (sec 20)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote']

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 21)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkTokenBalance frozenTokenId dodDao dodOwner1 100
  checkTokenBalance frozenTokenId dodDao dodOwner2 103

flushDecisionLambda
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushDecisionLambda originateFn = do
  consumer <- originateSimple "consumer" [] (contractConsumer)
  DaoOriginateData{..} <-
    originateFn ((decisionLambdaConfig consumer)
      >>- (ConfigDesc $ VotingPeriod 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 60)
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 10
        , vProposalKey = key1
        }
  -- Advance one voting period to a voting stage.
  advanceTime (sec 60)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote']

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 61)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  results <- fromVal <$> getStorage (toAddress consumer)
  assert (results == (#proposer <.!> [dodOwner1]))
    "Unexpected accepted proposals list"

flushFailOnExpiredProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushFailOnExpiredProposal originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (configWithRejectedProposal
       >>- (ConfigDesc (mkQuorumThreshold 1 50))
       >>- (ConfigDesc (VotingPeriod 20))
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceTime (sec 20)
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)
  _key2 <- createSampleProposal 2 dodOwner1 dodDao

  advanceTime (sec 41)
  -- `key1` is now expired, and `key2` is not yet expired.
  withSender dodAdmin $ call dodDao (Call @"Flush") 2
    & expectCustomErrorNoArg #eXPIRED_PROPOSAL dodDao

  -- `key1` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key1

  withSender dodAdmin $ call dodDao (Call @"Flush") 1
  checkTokenBalance frozenTokenId dodDao dodOwner1 110


dropProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
dropProposal originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (configWithRejectedProposal
       >>- (ConfigDesc (VotingPeriod 20))
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
       >>- (ConfigDesc (mkQuorumThreshold 1 50))
       >>- (ConfigDesc (VotingPeriod 20))
      )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)

  key1 <- createSampleProposal 1 dodOwner1 dodDao
  key2 <- createSampleProposal 2 dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceTime (sec 20)
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceTime (sec 20)

  key3 <- createSampleProposal 3 dodOwner1 dodDao

  -- `guardian` contract can drop any proposal.
  withSender dodOwner2 $ do
    call dodGuardian CallDefault (unTAddress dodDao, key1)

  -- `key2` is not yet expired since it has to be more than 60 seconds
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2
      & expectCustomErrorNoArg #dROP_PROPOSAL_CONDITION_NOT_MET dodDao

  advanceTime (sec 21)
  -- `key2` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2

  -- `key3` is not yet expired
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key3
      & expectCustomErrorNoArg #dROP_PROPOSAL_CONDITION_NOT_MET dodDao

  -- proposers can delete their proposal
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key3

  -- 30 tokens are frozen in total, but only 15 tokens are returned after drop_proposal
  checkTokenBalance frozenTokenId dodDao dodOwner1 115

proposalBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
proposalBoundedValue originateFn = do
  DaoOriginateData{..} <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts{ cmMaxProposals = Just 1 }
    )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $ do
    call dodDao (Call @"Propose") params
    call dodDao (Call @"Propose") params
      & expectCustomErrorNoArg #mAX_PROPOSALS_REACHED dodDao

votesBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
votesBoundedValue originateFn = do
  DaoOriginateData{..} <- originateFn
    ( voteConfig >>-
      ConfigDesc configConsts{ cmMaxVotes = Just 1 }
    )
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)
  key1 <- createSampleProposal 1 dodOwner2 dodDao
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
  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)
  withSender dodOwner1 $ do
    call dodDao (Call @"Vote") [downvote']
    call dodDao (Call @"Vote") [upvote']
      & expectCustomErrorNoArg #mAX_VOTES_REACHED dodDao
