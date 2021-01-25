-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains test on @vote@ entrypoint, shared for Lorentz and LIGO contracts.
module BaseDAO.ShareTest.Proposal.Vote
  ( module BaseDAO.ShareTest.Proposal.Vote
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test
import Morley.Nettest

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal.Config
import Lorentz.Contracts.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

voteNonExistingProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param [ProposeEp pm, VoteEp pm]
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
voteNonExistingProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn testConfig

  -- Create sample proposal
  _ <- createSampleProposal 1 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = HashUnsafe "\11\12\13"
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
    & expectFailed (toAddress dao) [mt|PROPOSAL_NOT_EXIST|]

voteMultiProposals
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
voteMultiProposals _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao
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

  callFrom (AddressResolved owner2) dao (Call @"Vote") params
  checkTokenBalance (unfrozenTokenId) dao owner2 95
  checkTokenBalance (frozenTokenId) dao owner2 5
  -- TODO [#31]: check storage if the vote update the proposal properly

insufficientTokenVote
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ProposalMetadataFromNum pm
    , ParameterContainsEntrypoints param [ProposeEp pm, VoteEp pm]
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
insufficientTokenVote _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao
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

  callFrom (AddressResolved owner2) dao (Call @"Vote") params
    & expectFailed (toAddress dao) [mt|VOTING_INSUFFICIENT_BALANCE|]

voteOutdatedProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
voteOutdatedProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, admin) <- originateFn testConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 20

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  advanceTime (sec 25)
  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
    & expectFailed (toAddress dao) [mt|VOTING_PERIOD_OVER|]

voteWithPermit
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
voteWithPermit _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  params <- permitProtect (AddressResolved owner1) =<< addDataToSign dao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dao owner1 12

voteWithPermitNonce
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
voteWithPermitNonce _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal
  key1 <- createSampleProposal 1 owner1 dao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Going to try calls with different nonces
  signed1@(_          , _) <- addDataToSign dao (Nonce 0) voteParam
  signed2@(dataToSign2, _) <- addDataToSign dao (Nonce 1) voteParam
  signed3@(_          , _) <- addDataToSign dao (Nonce 2) voteParam

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
