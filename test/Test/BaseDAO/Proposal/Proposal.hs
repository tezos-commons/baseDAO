-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoin logic for testing the Lorentz contract.
module Test.BaseDAO.Proposal.Proposal
  ( module Test.BaseDAO.Proposal.Proposal
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest

import Test.BaseDAO.Common
import Test.BaseDAO.Proposal.Config
import Lorentz.Contracts.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

validProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
validProposal originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  checkTokenBalance frozenTokenId dao owner1 10
  checkTokenBalance unfrozenTokenId dao owner1 90

  -- Check total supply
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid unfrozenTokenId)
      & expectError (VoidResult (190 :: Natural)) -- initial = 200

  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError (VoidResult (10 :: Natural)) -- initial = 0

  -- TODO [#31]: Currently proposalId is expected to be knowned (checkInStorage)

  -- TODO [#31]
  -- checkIfAProposalExist (makeProposalKey params owner1) dao

rejectProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
rejectProposal originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK

nonUniqueProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
nonUniqueProposal originateFn = do
  ((owner1, _), _, dao, _) <- originateFn testConfig
  _ <- createSampleProposal 1 owner1 dao
  createSampleProposal 1 owner1 dao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE

voteValidProposal
  :: forall pm param config caps base m.
    ( MonadNettest caps base m, ParameterC param pm, ProposalMetadataFromNum pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => (ConfigDesc config -> OriginateFn param m) -> m ()
voteValidProposal originateFn = do
  ((owner1, _), (owner2, _), dao, _) <- originateFn voteConfig

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
  checkTokenBalance (unfrozenTokenId) dao owner2 98
  checkTokenBalance (frozenTokenId) dao owner2 2
  -- TODO [#31]: check if the vote is updated properly
