-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE ApplicativeDo #-}
-- | Contains test on @vote@ entrypoint for the LIGO contract.
module Test.Ligo.BaseDAO.Proposal.Vote
  ( proposalCorrectlyTrackVotes
  , voteNonExistingProposal
  , voteDeletedProposal
  , voteMultiProposals
  , voteOutdatedProposal
  , voteValidProposal
  , voteWithPermit
  , voteWithPermitNonce
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Test.Cleveland
import Morley.Util.Named

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

voteNonExistingProposal
  :: (MonadCleveland caps base m, HasCallStack)
  => (ContractExtra -> ConfigDesc Config -> OriginateFn 'Base m) -> m ()
voteNonExistingProposal originateFn = do
  DaoOriginateData{..} <-
    originateFn testContractExtra testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  -- Advance three voting periods to a proposing stage.
  advanceToLevel (startLevel + 3*dodPeriod)
  -- Create sample proposal
  _ <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = UnsafeHash "\11\12\13"
        , vFrom = dodOwner2
        }
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 4*dodPeriod)

  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
    & expectFailedWith proposalNotExist

voteMultiProposals
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
voteMultiProposals originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 20)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 5)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + 3*dodPeriod)

  -- Create sample proposal
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 2
            , vProposalKey = key1
            , vFrom = dodOwner2
            }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            , vFrom = dodOwner2
            }
        ]

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 4*dodPeriod)
  withSender dodOwner2 $ call dodDao (Call @"Vote") params
  checkBalance dodDao dodOwner2 5
  getProposal dodDao key1 >>= \case
    Just proposal -> assert ((plUpvotes proposal) == 2) "Proposal had unexpected votes"
    Nothing -> error "Did not find proposal"

  getProposal dodDao key2 >>= \case
    Just proposal -> assert ((plDownvotes proposal) == 3) "Proposal had unexpected votes"
    Nothing -> error "Did not find proposal"

proposalCorrectlyTrackVotes
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
proposalCorrectlyTrackVotes originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  originationLevel <- getOriginationLevel dodDao

  let proposer = dodOwner1
  let voter1 = dodOwner2
  let voter2 = dodOperator1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 20)

  withSender voter1 $
    call dodDao (Call @"Freeze") (#amount :! 40)

  withSender voter2 $
    call dodDao (Call @"Freeze") (#amount :! 40)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (originationLevel + dodPeriod)

  -- Create sample proposal
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  let params1 = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 5
            , vProposalKey = key1
            , vFrom = voter1
            }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            , vFrom = voter1
            }
        ]

  let params2 = fmap NoPermit
        [ VoteParam
            { vVoteType = False
            , vVoteAmount = 2
            , vProposalKey = key1
            , vFrom = voter2
            }
        , VoteParam
            { vVoteType = True
            , vVoteAmount = 4
            , vProposalKey = key2
            , vFrom = voter2
            }
        ]

  let params3 = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 3
            , vProposalKey = key1
            , vFrom = voter1
            }
        , VoteParam
            { vVoteType = True
            , vVoteAmount = 3
            , vProposalKey = key2
            , vFrom = voter1
            }
        ]

  -- Advance one voting period to a voting stage.
  advanceToLevel (originationLevel + 2*dodPeriod)
  withSender voter1 . inBatch $ do
    call dodDao (Call @"Vote") params1
    call dodDao (Call @"Vote") params3
    pure ()

  withSender voter2  do
    call dodDao (Call @"Vote") params2

  proposal1 <- fromMaybe (error "proposal not found") <$> getProposal dodDao key1
  proposal2 <- fromMaybe (error "proposal not found") <$> getProposal dodDao key2

  assert (plUpvotes proposal1 == 8) "proposal did not track upvotes correctly"
  assert (plDownvotes proposal1 == 2) "proposal did not track downvotes correctly"

  assert (plUpvotes proposal2 == 7) "proposal did not track upvotes correctly"
  assert (plDownvotes proposal2 == 3) "proposal did not track downvotes correctly"

  voter1key1 <- getVoter dodDao (voter1, key1)
  voter1key2 <- getVoter dodDao (voter1, key2)
  voter2key1 <- getVoter dodDao (voter2, key1)
  voter2key2 <- getVoter dodDao (voter2, key2)

  voter1key1 @== Just 8 -- 8 upvotes, 0 downvotes

  voter1key2 @== Just 6 -- 3 upvotes, 3 downvotes

  voter2key1 @== Just 2 -- 0 upvotes, 2 downvotes

  voter2key2 @== Just 4 -- 4 upvotes, 0 downvotes


voteOutdatedProposal
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
voteOutdatedProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 4)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  startLevel <- getOriginationLevel dodDao
  runIO $ putTextLn $ show startLevel
  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner2
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)

  withSender dodOwner2 $ do
    call dodDao (Call @"Vote") [params]
    -- Advance two voting period to another voting stage.
    advanceToLevel (startLevel + 4*dodPeriod)
    call dodDao (Call @"Vote") [params]
      & expectFailedWith votingStageOver

voteValidProposal
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
voteValidProposal originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  startLevel <- getOriginationLevel dodDao
  -- Advance three voting period to a proposing stage.
  -- We skip three, instead of just one, because the freeze operations
  -- might extend into the next period, when tests are run on real network.
  -- makeing it unable to use those frozen tokens in the same period.
  advanceToLevel (startLevel + 3*dodPeriod)

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner2
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 4*dodPeriod)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
  checkBalance dodDao dodOwner2 2
  getProposal dodDao key1 >>= \case
    Just proposal -> assert ((plUpvotes proposal) == 2) "Proposal had unexpected votes"
    Nothing -> error "Did not find proposal"
  --
voteDeletedProposal
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
voteDeletedProposal originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  -- Advance three voting period to a proposing stage.
  advanceToLevel (startLevel + 3*dodPeriod)

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner2
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 4*dodPeriod)
  withSender dodOwner1 $ call dodDao (Call @"Drop_proposal") key1
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
    & expectFailedWith proposalNotExist

voteWithPermit
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
voteWithPermit originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 12)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  params <- permitProtect dodOwner1 =<< addDataToSign dodDao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)

  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
  checkBalance dodDao dodOwner1 12

voteWithPermitNonce
  :: (MonadCleveland caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
voteWithPermitNonce originateFn = do

  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  originationLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 60)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 50)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (originationLevel + dodPeriod)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (originationLevel + 2*dodPeriod)
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
      & expectFailedWith (missigned, (lPackValue dataToSign2))

    -- Nonce from future
    call dodDao (Call @"Vote") [params3]
      & expectFailedWith (missigned, (lPackValue dataToSign2))

    -- Good nonce after the previous successful entrypoint call
    call dodDao (Call @"Vote") [params2]

  -- Check counter
  (Nonce counter) <- getVotePermitsCounter dodDao
  counter @== 2
