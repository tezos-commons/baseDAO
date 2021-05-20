-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains test on @vote@ entrypoint for the LIGO contract.
module Test.Ligo.BaseDAO.Proposal.Vote
  ( voteNonExistingProposal
  , voteMultiProposals
  , voteOutdatedProposal
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test hiding (withSender)
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
  advanceTime (sec 10)
  -- Create sample proposal
  _ <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = UnsafeHash "\11\12\13"
        }
  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)

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
  advanceTime (sec 10)

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
  advanceTime (sec 10)
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
  advanceTime (sec 10)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)

  withSender dodOwner2 $ do
    call dodDao (Call @"Vote") [params]
    -- Advance two voting period to another voting stage.
    advanceTime (sec 25)
    call dodDao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_STAGE_OVER dodDao
