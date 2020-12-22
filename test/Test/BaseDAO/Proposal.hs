-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Time (sec)

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Lorentz.Test.Consumer
import Test.BaseDAO.ProposalConfig
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Proposal :: TestTree
test_BaseDAO_Proposal = testGroup "BaseDAO propose/vote entrypoints tests:"
  [ testGroup "Proposal creator:"
      [ nettestScenario "can propose a valid proposal" validProposal
      , nettestScenario "cannot propose an invalid proposal (rejected)"
          rejectProposal
      , nettestScenario "cannot propose with insufficient tokens"
          insufficientTokenProposal
      , nettestScenario "cannot propose a non-unique proposal" nonUniqueProposal
      ]
  , testGroup "Voter:"
      [ nettestScenario "can vote on a valid proposal"
          voteValidProposal
      , nettestScenario "cannot vote non-existing proposal"
          voteNonExistingProposal
      , nettestScenario "can vote on multiple proposals"
          voteMultiProposals
      , nettestScenario "cannot vote if the vote amounts exceeds token balance"
          insufficientTokenVote
      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "cannot vote on outdated proposal" $
          \_emulated -> voteOutdatedProposal
      , testGroup "Permit:"
          [ nettestScenario "can vote from another user's behalf"
              voteWithPermit
          , nettestScenario "counter works properly in permits"
              voteWithPermitNonce
          ]
      ]
  , testGroup "Admin:"
      [ nettestScenario "can set voting period" setVotingPeriod
      , nettestScenario "can set quorum threshold" setQuorumThreshold

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can flush proposals that got accepted" $
          \_emulated -> flushAcceptedProposals
      , nettestScenarioOnEmulator "can flush 2 proposals that got accepted" $
          \_emulated -> flushAcceptedProposalsWithAnAmount
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to not meeting quorum_threshold" $
          \_emulated -> flushRejectProposalQuorum
      , nettestScenarioOnEmulator "can flush proposals that got rejected due to negative votes" $
          \_emulated -> flushRejectProposalNegativeVotes
      , nettestScenario "flush should not affecting ongoing proposals"
          flushNotAffectOngoingProposals
      , nettestScenarioOnEmulator "flush with bad 'cRejectedProposalReturnValue'" $
          \_emulated -> flushWithBadConfig
      -- TODO [#15]: admin burn proposer token and test "flush"

      -- TODO [#38]: Improve this when contract size is smaller
      , nettestScenarioOnEmulator "flush and run decision lambda" $
          \_emulated -> flushDecisionLambda
      , nettestScenarioOnEmulator "can drop proposals" $
          \_emulated -> dropProposal
      ]
  , testGroup "Bounded Value"
      [ nettestScenario "bounded value on proposals" proposalBoundedValue
      , nettestScenario "bounded value on votes" votesBoundedValue
      , nettestScenario "bounded range on quorum_threshold" quorumThresholdBound
      , nettestScenario "bounded range on voting_period" votingPeriodBound
      ]
  ]

validProposal :: (Monad m) => NettestImpl m -> m ()
validProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config
  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 90
  -- TODO [#31]: Currently proposalId is expected to be knowned (checkInStorage)

  -- TODO [#31]
  -- checkIfAProposalExist (makeProposalKey params owner1) dao

rejectProposal :: (Monad m) => NettestImpl m -> m ()
rejectProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config
  let params = DAO.ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

insufficientTokenProposal :: (Monad m) => NettestImpl m -> m ()
insufficientTokenProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config
  let params = DAO.ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #pROPOSAL_INSUFFICIENT_BALANCE

nonUniqueProposal :: (Monad m) => NettestImpl m -> m ()
nonUniqueProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config
  _ <- createSampleProposal 1 owner1 dao
  createSampleProposal 1 owner1 dao
    & expectCustomError_ #pROPOSAL_NOT_UNIQUE

voteValidProposal :: (Monad m) => NettestImpl m -> m ()
voteValidProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 owner1 dao
  let params = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 98
  checkTokenBalance (DAO.frozenTokenId) dao owner2 2
  -- TODO [#31]: check if the vote is updated properly

voteNonExistingProposal :: (Monad m) => NettestImpl m -> m ()
voteNonExistingProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal
  _ <- createSampleProposal 1 owner1 dao
  let params = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = HashUnsafe "\11\12\13"
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
    & expectCustomError_ #pROPOSAL_NOT_EXIST

voteMultiProposals :: (Monad m) => NettestImpl m -> m ()
voteMultiProposals = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao
  let params = fmap DAO.NoPermit
        [ DAO.VoteParam
            { vVoteType = True
            , vVoteAmount = 2
            , vProposalKey = key1
            }
        , DAO.VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            }
        ]

  callFrom (AddressResolved owner2) dao (Call @"Vote") params
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 95
  checkTokenBalance (DAO.frozenTokenId) dao owner2 5
  -- TODO [#31]: check storage if the vote update the proposal properly

insufficientTokenVote :: (Monad m) => NettestImpl m -> m ()
insufficientTokenVote = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao
  let params = fmap DAO.NoPermit
        [ DAO.VoteParam
            { vVoteType = True
            , vVoteAmount = 51
            , vProposalKey = key1
            }
        , DAO.VoteParam
            { vVoteType = False
            , vVoteAmount = 50
            , vProposalKey = key1
            }
        ]

  callFrom (AddressResolved owner2) dao (Call @"Vote") params
    & expectCustomError_ #vOTING_INSUFFICIENT_BALANCE

voteOutdatedProposal :: (Monad m) => NettestImpl m -> m ()
voteOutdatedProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 20

  let params = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  advanceTime (sec 25)
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
    & expectCustomError_ #vOTING_PERIOD_OVER

voteWithPermit :: (Monad m) => NettestImpl m -> m ()
voteWithPermit = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  params <- permitProtect (AddressResolved owner1) =<< addDataToSign dao (DAO.Nonce 0)
        DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  checkTokenBalance DAO.frozenTokenId dao owner1 12

voteWithPermitNonce :: (Monad m) => NettestImpl m -> m ()
voteWithPermitNonce = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- | Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  let voteParam = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Going to try calls with different nonces
  signed1@(_          , _) <- addDataToSign dao (DAO.Nonce 0) voteParam
  signed2@(dataToSign2, _) <- addDataToSign dao (DAO.Nonce 1) voteParam
  signed3@(_          , _) <- addDataToSign dao (DAO.Nonce 2) voteParam

  params1 <- permitProtect (AddressResolved owner1) signed1
  params2 <- permitProtect (AddressResolved owner1) signed2
  params3 <- permitProtect (AddressResolved owner1) signed3

  -- Good nonce
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params1]

  -- Outdated nonce
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params1]
    & expectCustomError #mISSIGNED (checkedCoerce $ lPackValue dataToSign2)

  -- Nonce from future
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params3]
    & expectCustomError #mISSIGNED (checkedCoerce $ lPackValue dataToSign2)

  -- Good nonce after the previous successful entrypoint call
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params2]

  -- Check counter
  consumer <- originateSimple "consumer" [] contractConsumer
  callFrom (AddressResolved owner1) dao (Call @"GetVotePermitCounter") (mkView () consumer)
  checkStorage (AddressResolved $ toAddress consumer) (toVal [2 :: Natural])

setVotingPeriod :: (Monad m) => NettestImpl m -> m ()
setVotingPeriod = uncapsNettest $ do
  ((owner1, _), _, dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () config

  let param = 60 * 60 -- 1 hour

  callFrom (AddressResolved owner1) dao (Call @"Set_voting_period") param
    & expectCustomError_ #nOT_ADMIN

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") param
  -- TODO [#31]: checkStorage

setQuorumThreshold :: (Monad m) => NettestImpl m -> m ()
setQuorumThreshold = uncapsNettest $ do
  ((owner1, _), _, dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () config

  let param = 100

  callFrom (AddressResolved owner1) dao (Call @"Set_quorum_threshold") param
    & expectCustomError_ #nOT_ADMIN

  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") param
  -- TODO [#31]: checkStorage

flushNotAffectOngoingProposals :: (Monad m) => NettestImpl m -> m ()
flushNotAffectOngoingProposals = uncapsNettest $ do
  ((owner1, _), _, dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- Note: Cannot set to few seconds, since in real network, each
  -- calls takes some times to run. 20 seconds seem to be the ideal.
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") (2 * 60)

  advanceTime (sec 3)

  _key1 <- createSampleProposal 1 owner1 dao
  _key2 <- createSampleProposal 2 owner1 dao
  callFrom (AddressResolved admin) dao (Call @"Flush") Nothing

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  -- checkIfAProposalExist (key2 :: ByteString) dao

flushAcceptedProposals :: (Monad m) => NettestImpl m -> m ()
flushAcceptedProposals = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () config

  -- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1

  -- | Accepted Proposals
  key1 <- createSampleProposal 1 owner1 dao

  let upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }
      downvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote, downvote]

  -- | Checking balance of proposer and voters
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10
  checkTokenBalance (DAO.frozenTokenId) dao owner2 3
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 97

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") Nothing

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 100 -- proposer

  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

flushAcceptedProposalsWithAnAmount :: (Monad m) => NettestImpl m -> m ()
flushAcceptedProposalsWithAnAmount = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig @Integer () config

  -- | Accepted Proposals
  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao
  advanceTime (sec 1)
  key3 <- createSampleProposal 3 owner1 dao


  let vote key = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key
        }

  advanceTime (sec 21)

  key4 <- createSampleProposal 4 owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 40
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 60

  callFrom (AddressResolved admin) dao (Call @"Flush") (Just 2)

  -- Proposals are flushed
  callFrom (AddressResolved owner2) dao (Call @"Vote") [vote key1]
    & expectCustomError_ #vOTING_PERIOD_OVER
  callFrom (AddressResolved owner2) dao (Call @"Vote") [vote key2]
    & expectCustomError_ #vOTING_PERIOD_OVER

  -- Proposal is over but not affected
  callFrom (AddressResolved owner2) dao (Call @"Vote") [vote key3]
    & expectCustomError_ #vOTING_PERIOD_OVER

  -- Proposal is not yet over
  callFrom (AddressResolved owner2) dao (Call @"Vote") [vote key4]

  -- Only 2 proposals are flush, so only 20 tokens are unfrozen back.
  checkTokenBalance (DAO.frozenTokenId) dao owner1 20
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 80

flushRejectProposalQuorum :: (Monad m) => NettestImpl m -> m ()
flushRejectProposalQuorum =
  uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateBaseDaoWithConfig @Integer @Empty () configWithRejectedProposal

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3

  -- | Rejected Proposal
  key1 <- createSampleProposal 1 owner1 dao

  let votes = fmap DAO.NoPermit
        [ DAO.VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , DAO.VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        ]
  callFrom (AddressResolved owner2) dao (Call @"Vote") votes

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") Nothing

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 95 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

flushRejectProposalNegativeVotes :: (Monad m) => NettestImpl m -> m ()
flushRejectProposalNegativeVotes = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateBaseDaoWithConfig @Integer @Empty () configWithRejectedProposal

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3

  -- | Rejected Proposal
  key1 <- createSampleProposal 1 owner1 dao

  let votes = fmap DAO.NoPermit
        [ DAO.VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , DAO.VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , DAO.VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        ]
  callFrom (AddressResolved owner2) dao (Call @"Vote") votes

  -- Check proposer balance
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") Nothing

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 95 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

flushWithBadConfig :: (Monad m) => NettestImpl m -> m ()
flushWithBadConfig = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () badRejectedValueConfig

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2

  key1 <- createSampleProposal 1 owner1 dao

  let upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]

  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 90
  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") Nothing

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 90 -- slash all frozen values
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100

flushDecisionLambda :: (Monad m) => NettestImpl m -> m ()
flushDecisionLambda = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig @Integer @Empty () decisionLambdaConfig

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1

  key1 <- createSampleProposal 1 owner1 dao

  let upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") Nothing

  -- | Credit the proposer 10 tokens when the proposal is accepted
  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 110

proposalBoundedValue :: (Monad m) => NettestImpl m -> m ()
proposalBoundedValue = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig @Integer @Empty () (config { DAO.cMaxProposals = 1})

  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #mAX_PROPOSALS_REACHED

votesBoundedValue :: (Monad m) => NettestImpl m -> m ()
votesBoundedValue = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDaoWithConfig @Integer @Empty () (config { DAO.cMaxVotes = 1})

  key1 <- createSampleProposal 1 owner2 dao
  let upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
      downvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner1) dao (Call @"Vote") [downvote]
  callFrom (AddressResolved owner1) dao (Call @"Vote") [upvote]
    & expectCustomError_ #mAX_VOTES_REACHED

quorumThresholdBound :: (Monad m) => NettestImpl m -> m ()
quorumThresholdBound = uncapsNettest $ do
  (_, _, dao, admin) <- originateBaseDaoWithConfig @Integer @Empty ()
                                          (config { DAO.cMinQuorumThreshold = 1
                                                  , DAO.cMaxQuorumThreshold = 2
                                                  })
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 0
    & expectCustomError_ #oUT_OF_BOUND_QUORUM_THRESHOLD
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3
    & expectCustomError_ #oUT_OF_BOUND_QUORUM_THRESHOLD

votingPeriodBound :: (Monad m) => NettestImpl m -> m ()
votingPeriodBound = uncapsNettest $ do
  (_, _, dao, admin) <- originateBaseDaoWithConfig @Integer @Empty ()
                                          (config { DAO.cMinVotingPeriod = 1
                                                  , DAO.cMaxVotingPeriod = 2
                                                  })
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 1
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 2
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 0
    & expectCustomError_ #oUT_OF_BOUND_VOTING_PERIOD
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 3
    & expectCustomError_ #oUT_OF_BOUND_VOTING_PERIOD

dropProposal :: (Monad m) => NettestImpl m -> m ()
dropProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig @Integer () badRejectedValueConfig

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 20
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2

  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao

  let params key = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params key1]
  advanceTime (sec 20)

  key3 <- createSampleProposal 3 owner1 dao

  callFrom (AddressResolved admin) dao (Call @"Drop_proposal") key1
  callFrom (AddressResolved admin) dao (Call @"Drop_proposal") key2
    & expectCustomError_ #fAIL_DROP_PROPOSAL_NOT_ACCEPTED
  callFrom (AddressResolved admin) dao (Call @"Drop_proposal") key3
    & expectCustomError_ #fAIL_DROP_PROPOSAL_NOT_OVER

  -- 30 tokens are frozen in total, but 10 tokens are returned after drop_proposal
  checkTokenBalance (DAO.frozenTokenId) dao owner1 20
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 80


-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

createSampleProposal
  :: MonadNettest caps base m
  => Integer -> Address -> TAddress (DAO.Parameter Integer Empty) -> m (DAO.ProposalKey Integer)
createSampleProposal counter owner1 dao = do
  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = counter
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)

-- TODO: Implement this via [#31] instead
-- checkIfAProposalExist
--   :: MonadNettest caps base m
--   => DAO.ProposalKey -> TAddress (DAO.Parameter TestProposalMetadata) -> m ()
-- checkIfAProposalExist proposalKey dao = do
--   owner :: Address <- newAddress "owner"
--   consumer <- originateSimple "consumer" [] contractConsumer
--   -- | If the proposal exists, there should be no error
--   callFrom (AddressResolved owner) dao (Call @"Proposal_metadata") (mkView proposalKey consumer)

-- TODO [#31]: See this ISSUES: https://gitlab.com/morley-framework/morley/-/issues/415#note_435327096
-- Check if certain field in storage
-- checkPropertyOfProposal :: _
-- checkPropertyOfProposal = error "undefined"
