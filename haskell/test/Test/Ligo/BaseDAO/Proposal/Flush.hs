-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
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
  , flushNotEmpty
  ) where

import Universum

import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

flushAcceptedProposals
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
flushAcceptedProposals originateFn = do
-- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  DaoOriginateData{..} <- originateFn (testConfig
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      ) defaultQuorumThreshold

  withSender dodOwner2 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 15)

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- Accepted Proposals
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)

  let upvote' = NoPermit VoteParam
        { vFrom = toAddress dodOwner2
        , vVoteType = True
        , vVoteAmount = 10
        , vProposalKey = key1
        }
      downvote' = NoPermit VoteParam
        { vFrom = toAddress dodOwner2
        , vVoteType = False
        , vVoteAmount = 5
        , vProposalKey = key1
        }
  withSender dodOwner2 $
    transfer dodDao $ calling (ep @"Vote") [upvote', downvote']

  -- Checking freezing histories of proposer and voters
  fhOwner1 <- getFreezeHistory dodDao dodOwner1
  fhOwner1 @== Just AddressFreezeHistory
    { fhCurrentStageNum = 1
    , fhStaked = 10
    , fhCurrentUnstaked = 0
    , fhPastUnstaked = 0
    }
  fhOwner2 <- getFreezeHistory dodDao dodOwner2
  fhOwner2 @== Just AddressFreezeHistory
    { fhCurrentStageNum  = 2
    , fhStaked = 15
    , fhCurrentUnstaked = 0
    , fhPastUnstaked = 0
    }

  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod)
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 100
  withSender dodOwner1 $ (transfer dodDao $ calling (ep @"Unstake_vote") [key1])
    & expectFailedWith voterDoesNotExist
  withSender dodOwner2 $ transfer dodDao $ calling (ep @"Unstake_vote") [key1]

  checkIfAProposalExist key1 dodDao False

  fhOwner1' <- getFreezeHistory dodDao dodOwner1
  fhOwner1' @== Just AddressFreezeHistory
    { fhCurrentStageNum = 3
    , fhStaked = 0
    , fhCurrentUnstaked = 0
    , fhPastUnstaked = 10
    }
  fhOwner2' <- getFreezeHistory dodDao dodOwner2
  fhOwner2' @== Just AddressFreezeHistory
    { fhCurrentStageNum = 3
    , fhStaked = 0
    , fhCurrentUnstaked = 0
    , fhPastUnstaked = 15
    }

flushAcceptedProposalsWithAnAmount
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
flushAcceptedProposalsWithAnAmount originateFn = do
  DaoOriginateData{..}
    <- originateFn (testConfig
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 20 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 30 })
        ) defaultQuorumThreshold
  originationLevel <- getOriginationLevel' @'Base dodDao

  -- [Voting]
  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 30)

  withSender dodOwner2 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 10)

  startLevel <- getOriginationLevel' @'Base dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- [Proposing]
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  advanceLevel 1
  key3 <- createSampleProposal 3 dodOwner1 dodDao

  let vote' key = NoPermit VoteParam
        { vFrom = toAddress dodOwner2
        , vVoteType = True
        , vVoteAmount = 5
        , vProposalKey = key
        }

  advanceToLevel (originationLevel + 2 * dodPeriod)

  -- [Voting]
  withSender dodOwner2 . inBatch $ do
      transfer dodDao $ calling (ep @"Vote") [vote' key1]
      transfer dodDao $ calling (ep @"Vote") [vote' key2]
      pure ()

  proposalStart2 <- getProposalStartLevel' @'Base dodDao key2
  proposalStart3 <- getProposalStartLevel' @'Base dodDao key3
  advanceToLevel (proposalStart2 + 21)

  -- [Proposing]
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 2

  -- key1 and key2 are flushed. (Tokens remain the same, because they are all passed)
  checkBalance' @'Base dodDao dodOwner1 30

  advanceToLevel (proposalStart3 + 21)
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 1

  ---- key3 is rejected
  checkBalance' @'Base dodDao dodOwner1 29

flushRejectProposalQuorum
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
flushRejectProposalQuorum originateFn = do
  DaoOriginateData{..}
    <- originateFn (testConfig
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 20 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        ) (mkQuorumThreshold 3 5)

  withSender dodOwner2 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 5)

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel' @'Base dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 3
          , vProposalKey = key1
          , vFrom = toAddress dodOwner2
          }
        ]
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOwner2 $ transfer dodDao $ calling (ep @"Vote") votes

  proposalStart <- getProposalStartLevel' @'Base dodDao key1
  advanceToLevel (proposalStart + 20)
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 100

  checkIfAProposalExist' @'Base key1 dodDao False

  checkBalance' @'Base dodDao dodOwner1 09
  checkBalance' @'Base dodDao dodOwner2 05 -- Since voter tokens are not burned

flushRejectProposalNegativeVotes
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
flushRejectProposalNegativeVotes originateFn = do
  DaoOriginateData{..}
    <- originateFn (testConfig
          >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 20 })
          >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
          ) (mkQuorumThreshold 3 100)

  withSender dodOwner2 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 3)

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel' @'Base dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- Rejected Proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          , vFrom = toAddress dodOwner2
          }
        , VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          , vFrom = toAddress dodOwner2
          }
        , VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          , vFrom = toAddress dodOwner2
          }
        ]
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOwner2 $ transfer dodDao$ calling (ep @"Vote") votes

  -- Check proposer balance
  checkBalance' @'Base dodDao dodOwner1 10

  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel' @'Base dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod)
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 100

  checkIfAProposalExist' @'Base key1 dodDao False

  checkBalance' @'Base dodDao dodOwner1 09
  checkBalance' @'Base dodDao dodOwner2 03

flushFailOnExpiredProposal
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
flushFailOnExpiredProposal originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (testConfig
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 80 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 130 })
       >>- (ConfigDesc configConsts{ cmPeriod = Just 40 })
      ) (mkQuorumThreshold 1 5000)
  originationLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 20)

  withSender dodOwner2 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (originationLevel + dodPeriod)
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceToLevel (originationLevel + 2*dodPeriod)
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        , vFrom = toAddress dodOwner2
        }
  withSender dodOwner2 $ transfer dodDao $ calling (ep @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceToLevel (originationLevel + 3*dodPeriod)
  _key2 <- createSampleProposal 2 dodOwner1 dodDao

  advanceToLevel (originationLevel + 5*dodPeriod + 1)
  -- `key1` is now expired, and `key2` is not yet expired.
  withSender dodAdmin $ (transfer dodDao $ calling (ep @"Flush") 2)
    & expectFailedWith expiredProposal

  -- `key1` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    transfer dodDao $ calling (ep @"Drop_proposal") key1

  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 1
  checkBalance dodDao dodOwner1 18 -- One proposal dropped, one rejected.

flushProposalFlushTimeNotReach
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
flushProposalFlushTimeNotReach originateFn = do
  DaoOriginateData{..} <-
    originateFn (testConfig
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 20 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 50 })
        ) defaultQuorumThreshold

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 30)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  -- Advance two voting period to another proposing stage.
  advanceToLevel (startLevel + 3*dodPeriod + 1) -- skip voting period
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  proposalStart <- getProposalStartLevel dodDao key2
  advanceToLevel (proposalStart + 20)
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 100
  checkBalance dodDao dodOwner1 (9 + 9 + 10) -- first 2 proposals got flushed then slashed by 1, the last one is not affected.

  checkIfAProposalExist key1 dodDao False
  checkIfAProposalExist key2 dodDao False

flushNotEmpty
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
flushNotEmpty originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (testConfig
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 120 })
      ) (mkQuorumThreshold 1 50)

  originationLevel <- getOriginationLevel dodDao

  -- no proposal exist at this point, so flush is empty
  withSender dodAdmin $ (transfer dodDao $ calling (ep @"Flush") 1)
    & expectFailedWith emptyFlush

  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Freeze") (#amount :! 20)
  withSender dodOwner2 $ transfer dodDao$ calling (ep @"Freeze") (#amount :! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (originationLevel + dodPeriod)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  proposalStart <- getProposalStartLevel dodDao key1

  -- Advance one voting period to a voting stage.
  advanceToLevel (originationLevel + 2*dodPeriod)
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        , vFrom = toAddress dodOwner2
        }
  withSender dodOwner2 $ transfer dodDao$ calling (ep @"Vote") [params key1]

  advanceToLevel (originationLevel + 3*dodPeriod - 5)
  -- the proposal exists at this point (and has votes), but it can't be flushed
  -- yet, because it needs some more level to meet the `proposal_flush_time`
  withSender dodAdmin $ (transfer dodDao $ calling (ep @"Flush") 1)
    & expectFailedWith emptyFlush

  -- however after enough levels are past flushing is allowed
  advanceToLevel (proposalStart + 42)
  withSender dodAdmin $ transfer dodDao $ calling (ep @"Flush") 1
