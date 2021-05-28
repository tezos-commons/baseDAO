-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# LANGUAGE ApplicativeDo #-}

-- | Contains tests on @flush@ entrypoint logic.
module Test.Ligo.BaseDAO.Proposal.Flush
  ( flushProposalFlushTimeNotReach
  , flushAcceptedProposalsWithAnAmount
  , flushAcceptedProposals
  , flushRejectProposalQuorum
  , flushRejectProposalNegativeVotes
  , flushFailOnExpiredProposal
  , flushWithBadConfig
  , flushDecisionLambda
  ) where

import Universum

import Lorentz.Test (contractConsumer)
import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

flushAcceptedProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetTotalSupplyFn m -> m ()
flushAcceptedProposals originateFn getTotalSupplyFn = do
-- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  DaoOriginateData{..} <- originateFn (testConfig
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 60

  -- Accepted Proposals
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  -- Advance one voting period to a voting stage.
  advanceLevel 65

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
  advanceLevel 61
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
        >>- (ConfigDesc $ Period 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        )

  -- [Voting]
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 6)

  advanceLevel 20

  -- [Proposing]
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  advanceLevel 1
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  let vote' key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 3
        , vProposalKey = key
        }

  advanceLevel 20

  -- [Voting]
  withSender dodOwner2 . inBatch $ do
      call dodDao (Call @"Vote") [vote' key1]
      call dodDao (Call @"Vote") [vote' key2]
      pure ()

  advanceLevel 22

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
        >>- (ConfigDesc $ Period 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 5)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 20

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
  advanceLevel 20
  withSender dodOwner2 $ call dodDao (Call @"Vote") votes

  -- Advance one voting period to a proposing stage.
  advanceLevel 21
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
          >>- (ConfigDesc (Period 20))
          >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
          >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
          >>- (ConfigDesc (mkQuorumThreshold 3 100))
          )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 20

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
  advanceLevel 20
  withSender dodOwner2 $ call dodDao (Call @"Vote") votes

  -- Check proposer balance
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Advance one voting period to a proposing stage.
  advanceLevel 21
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
      >>- (ConfigDesc (Period 20))
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      >>- (ConfigDesc (mkQuorumThreshold 1 2))
      )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 20
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  -- Advance one voting period to a voting stage.
  advanceLevel 20
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote']

  -- Advance one voting period to a proposing stage.
  advanceLevel 21
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
  consumer <- originateSimple "consumer" [] contractConsumer
  DaoOriginateData{..} <-
    originateFn ((decisionLambdaConfig consumer)
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      )

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 10
        , vProposalKey = key1
        }
  -- Advance one voting period to a voting stage.
  advanceLevel 60
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote']

  -- Advance one voting period to a proposing stage.
  advanceLevel 61
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
       >>- (ConfigDesc (Period 20))
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceLevel 20
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceLevel 20
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceLevel 20
  _key2 <- createSampleProposal 2 dodOwner1 dodDao

  advanceLevel 41
  -- `key1` is now expired, and `key2` is not yet expired.
  withSender dodAdmin $ call dodDao (Call @"Flush") 2
    & expectCustomErrorNoArg #eXPIRED_PROPOSAL dodDao

  -- `key1` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key1

  withSender dodAdmin $ call dodDao (Call @"Flush") 1
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

flushProposalFlushTimeNotReach
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushProposalFlushTimeNotReach originateFn = do
  DaoOriginateData{..} <-
    originateFn (configWithRejectedProposal
        >>- (ConfigDesc $ Period 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  -- Advance one voting period to a proposing stage.
  advanceLevel 20

  (_key1, _key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  -- Advance two voting period to another proposing stage.
  advanceLevel 20 -- skip voting period
  advanceLevel 21
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  checkTokenBalance (frozenTokenId) dodDao dodOwner1 (100 + 5 + 5 + 10) -- first 2 proposals got flushed then slashed by 5, the last one is not affected.

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  -- checkIfAProposalExist (key2 :: ByteString) dodDao
