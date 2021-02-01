-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on proposal/vote limits logic for testing Lorentz
-- and Ligo contracts.
module BaseDAO.ShareTest.Proposal.Bounds
  ( module BaseDAO.ShareTest.Proposal.Bounds
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal.Config
import Lorentz.Contracts.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

setVotingPeriod
  :: forall pm param config caps base m.
    ( MonadNettest caps base m
    , ParameterC param pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
setVotingPeriod _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  let param = 60 * 60 -- 1 hour

  withSender (AddressResolved owner1) $ call dao (Call @"Set_voting_period") param
    & expectCustomErrorUnit #nOT_ADMIN

  withSender (AddressResolved admin) $ call dao (Call @"Set_voting_period") param
  -- TODO [#31]: checkStorage

setQuorumThreshold
  :: forall pm param config caps base m.
    ( MonadNettest caps base m
    , ParameterC param pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
setQuorumThreshold _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  let param = 100

  withSender (AddressResolved owner1) $ call dao (Call @"Set_quorum_threshold") param
    & expectCustomErrorUnit #nOT_ADMIN

  withSender (AddressResolved admin) $ call dao (Call @"Set_quorum_threshold") param
  -- TODO [#31]: checkStorage

proposalBoundedValue
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param '[ProposeEp pm]
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
proposalBoundedValue _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts{ cmMaxProposals = Just 1 }
    )

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorUnit #mAX_PROPOSALS_REACHED

votesBoundedValue
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param '[ProposeEp pm, VoteEp pm]
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
votesBoundedValue _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn
    ( voteConfig >>-
      ConfigDesc configConsts{ cmMaxVotes = Just 1 }
    )

  key1 <- createSampleProposal 1 owner2 dao
  let upvote = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
      downvote = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Vote") [downvote]
  withSender (AddressResolved owner1) $ call dao (Call @"Vote") [upvote]
    & expectCustomErrorUnit #mAX_VOTES_REACHED

quorumThresholdBound
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
quorumThresholdBound _ originateFn = withFrozenCallStack $ do
  (_, _, dao, admin) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts
        { cmMinQuorumThreshold = Just 1
        , cmMaxQuorumThreshold = Just 2
        }
    )
  withSender (AddressResolved admin) $ call dao (Call @"Set_quorum_threshold") 1
  withSender (AddressResolved admin) $ call dao (Call @"Set_quorum_threshold") 2
  withSender (AddressResolved admin) $ call dao (Call @"Set_quorum_threshold") 0
    & expectCustomErrorText #oUT_OF_BOUND_QUORUM_THRESHOLD
  withSender (AddressResolved admin) $ call dao (Call @"Set_quorum_threshold") 3
    & expectCustomErrorText #oUT_OF_BOUND_QUORUM_THRESHOLD

votingPeriodBound
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
votingPeriodBound _ originateFn = do
  (_, _, dao, admin) <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts
        { cmMinVotingPeriod = Just 1
        , cmMaxVotingPeriod = Just 2
        }
    )
  withSender (AddressResolved admin) $ call dao (Call @"Set_voting_period") 1
  withSender (AddressResolved admin) $ call dao (Call @"Set_voting_period") 2
  withSender (AddressResolved admin) $ call dao (Call @"Set_voting_period") 0
    & expectCustomErrorUnit #oUT_OF_BOUND_VOTING_PERIOD
  withSender (AddressResolved admin) $ call dao (Call @"Set_voting_period") 3
    & expectCustomErrorUnit #oUT_OF_BOUND_VOTING_PERIOD
