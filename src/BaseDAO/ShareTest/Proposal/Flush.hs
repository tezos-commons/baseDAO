-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains test on @flush@ and @dropProposal@ entrypoints,
-- shared for Lorentz and LIGO contracts.
module BaseDAO.ShareTest.Proposal.Flush
  ( module BaseDAO.ShareTest.Proposal.Flush
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test
import Morley.Nettest

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal.Config
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

flushNotAffectOngoingProposals
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param
       '[ "Set_voting_period" :> VotingPeriod
        , FlushEp
        , ProposeEp pm
        ]
  , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushNotAffectOngoingProposals _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  -- Note: Cannot set to few seconds, since in real network, each
  -- calls takes some times to run. 20 seconds seem to be the ideal.
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") (2 * 60)

  advanceTime (sec 3)

  _key1 <- createSampleProposal 1 owner1 dao
  _key2 <- createSampleProposal 2 owner1 dao
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  -- checkIfAProposalExist (key2 :: ByteString) dao

flushAcceptedProposals
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushAcceptedProposals _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig

  -- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1

  -- | Accepted Proposals
  key1 <- createSampleProposal 1 owner1 dao

  let upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }
      downvote = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote, downvote]

  -- | Checking balance of proposer and voters
  checkTokenBalance (frozenTokenId) dao owner1 10
  checkTokenBalance (frozenTokenId) dao owner2 3
  checkTokenBalance (unfrozenTokenId) dao owner2 97

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 100 -- proposer

  checkTokenBalance (frozenTokenId) dao owner2 0
  checkTokenBalance (unfrozenTokenId) dao owner2 100 -- voter

flushAcceptedProposalsWithAnAmount
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param
       '[ ProposeEp pm
        , VoteEp pm
        , FlushEp
        ]
    , FA2.ParameterC param
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushAcceptedProposalsWithAnAmount _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig

  -- | Accepted Proposals
  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao
  advanceTime (sec 1)
  key3 <- createSampleProposal 3 owner1 dao


  let vote key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key
        }

  advanceTime (sec 21)

  key4 <- createSampleProposal 4 owner1 dao

  checkTokenBalance (frozenTokenId) dao owner1 40
  checkTokenBalance (unfrozenTokenId) dao owner1 60

  callFrom (AddressResolved admin) dao (Call @"Flush") 2

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
  checkTokenBalance (frozenTokenId) dao owner1 20
  checkTokenBalance (unfrozenTokenId) dao owner1 80

flushRejectProposalQuorum
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushRejectProposalQuorum _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn configWithRejectedProposal

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3

  -- | Rejected Proposal
  key1 <- createSampleProposal 1 owner1 dao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        , VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          }
        ]
  callFrom (AddressResolved owner2) dao (Call @"Vote") votes

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 95 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (frozenTokenId) dao owner2 0
  checkTokenBalance (unfrozenTokenId) dao owner2 100 -- voter

flushRejectProposalNegativeVotes
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushRejectProposalNegativeVotes _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin)
    <- originateFn configWithRejectedProposal

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 3

  -- | Rejected Proposal
  key1 <- createSampleProposal 1 owner1 dao

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
  callFrom (AddressResolved owner2) dao (Call @"Vote") votes

  -- Check proposer balance
  checkTokenBalance (frozenTokenId) dao owner1 10

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 95 -- proposer: cRejectedValue reduce frozen token by half
  checkTokenBalance (frozenTokenId) dao owner2 0
  checkTokenBalance (unfrozenTokenId) dao owner2 100 -- voter

flushWithBadConfig
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushWithBadConfig _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn badRejectedValueConfig

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2

  key1 <- createSampleProposal 1 owner1 dao

  let upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]

  checkTokenBalance (unfrozenTokenId) dao owner1 90
  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dao
  --   & expectCustomError_ #pROPOSAL_NOT_EXIST

  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 90 -- slash all frozen values
  checkTokenBalance (frozenTokenId) dao owner2 0
  checkTokenBalance (unfrozenTokenId) dao owner2 100

flushDecisionLambda
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param
       '[ "Set_voting_period" :> VotingPeriod
        , "Set_quorum_threshold" :> QuorumThreshold
        , ProposeEp pm
        , VoteEp pm
        , FlushEp
        ]
    , FA2.ParameterC param
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
flushDecisionLambda _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn decisionLambdaConfig

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 60
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1

  key1 <- createSampleProposal 1 owner1 dao

  let upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]

  advanceTime (sec 61)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  -- | Credit the proposer 10 tokens when the proposal is accepted
  checkTokenBalance (frozenTokenId) dao owner1 0
  checkTokenBalance (unfrozenTokenId) dao owner1 110

dropProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
dropProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn badRejectedValueConfig

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 20
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 2

  key1 <- createSampleProposal 1 owner1 dao
  key2 <- createSampleProposal 2 owner1 dao

  let params key = NoPermit VoteParam
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
  checkTokenBalance (frozenTokenId) dao owner1 20
  checkTokenBalance (unfrozenTokenId) dao owner1 80
