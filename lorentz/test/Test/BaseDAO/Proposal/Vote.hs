-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains test on @vote@ entrypoint of the Lorentz contract.
module Test.BaseDAO.Proposal.Vote
  ( module Test.BaseDAO.Proposal.Vote
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test hiding (withSender)
import Morley.Nettest

import Test.BaseDAO.Common
import Test.BaseDAO.Proposal.Config
import Lorentz.Contracts.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

voteNonExistingProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param [ProposeEp pm, VoteEp pm]
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
voteNonExistingProposal originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn testConfig

  -- Create sample proposal
  _ <- createSampleProposal 1 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = HashUnsafe "\11\12\13"
        }

  withSender owner2 $ call dao (Call @"Vote") [params]
    & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

voteMultiProposals
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
voteMultiProposals originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao -- Don't advance time.
  key2 <- createSampleProposal 2 owner1 dao
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

  withSender owner2 $ call dao (Call @"Vote") params
  checkTokenBalance (unfrozenTokenId) dao owner2 95
  checkTokenBalance (frozenTokenId) dao owner2 5
  -- TODO [#31]: check storage if the vote update the proposal properly

voteOutdatedProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
voteOutdatedProposal originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn testConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  withSender owner2 $ do
    call dao (Call @"Vote") [params]
    advanceTime (sec 25)
    call dao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER dao
