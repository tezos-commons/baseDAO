-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoin logic for testing Lorentz
-- and Ligo contracts.
module BaseDAO.ShareTest.Proposal.Proposal
  ( module BaseDAO.ShareTest.Proposal.Proposal
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal.Config
import Lorentz.Contracts.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

validProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
validProposal  _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  checkTokenBalance frozenTokenId dao owner1 10
  checkTokenBalance unfrozenTokenId dao owner1 90
  -- TODO [#31]: Currently proposalId is expected to be knowned (checkInStorage)

  -- TODO [#31]
  -- checkIfAProposalExist (makeProposalKey params owner1) dao

rejectProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
rejectProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

insufficientTokenProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
insufficientTokenProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
    & expectCustomError_ #pROPOSAL_INSUFFICIENT_BALANCE

nonUniqueProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
nonUniqueProposal _ originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  _ <- createSampleProposal 1 owner1 dao
  createSampleProposal 1 owner1 dao
    & expectCustomError_ #pROPOSAL_NOT_UNIQUE

voteValidProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
voteValidProposal _ originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [params]
  checkTokenBalance (unfrozenTokenId) dao owner2 98
  checkTokenBalance (frozenTokenId) dao owner2 2
  -- TODO [#31]: check if the vote is updated properly
