-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains test on @vote@ entrypoint for the LIGO contract.
module Test.Ligo.BaseDAO.Proposal.Vote
  ( voteNonExistingProposal
  , voteMultiProposals
  , voteOutdatedProposal
  , voteValidProposal
  , voteWithPermit
  , voteWithPermitNonce
  , votesBoundedValue
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

voteNonExistingProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteNonExistingProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  -- Create sample proposal
  _ <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = UnsafeHash "\11\12\13"
        }
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod

  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
    & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

voteMultiProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteMultiProposals originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 5)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  key2 <- createSampleProposal 2 dodOwner1 dodDao
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

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") params
  checkTokenBalance (frozenTokenId) dodDao dodOwner2 105
  -- TODO [#31]: check storage if the vote update the proposal properly

voteOutdatedProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteOutdatedProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod

  withSender dodOwner2 $ do
    call dodDao (Call @"Vote") [params]
    -- Advance two voting period to another voting stage.
    advanceLevel (2 * dodPeriod)
    call dodDao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_STAGE_OVER dodDao

voteValidProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteValidProposal originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel 10
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dodDao dodOwner2 102
  -- TODO [#31]: check if the vote is updated properly

voteWithPermit
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteWithPermit originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 12)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  params <- permitProtect dodOwner1 =<< addDataToSign dodDao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel 10

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
  advanceLevel 10

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel 10
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
  advanceLevel 10
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
  advanceLevel 10
  withSender dodOwner1 $ do
    call dodDao (Call @"Vote") [downvote']
    call dodDao (Call @"Vote") [upvote']
      & expectCustomErrorNoArg #mAX_VOTES_REACHED dodDao
