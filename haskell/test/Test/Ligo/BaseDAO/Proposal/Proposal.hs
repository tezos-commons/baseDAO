-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoin logic for testing the Ligo contract.
module Test.Ligo.BaseDAO.Proposal.Proposal
  ( validProposal
  , rejectProposal
  , nonUniqueProposal
  , voteValidProposal
  ) where

import Universum

import Time (sec)

import Lorentz hiding ((>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

validProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetTotalSupplyFn m -> m ()
validProposal originateFn getTotalSupplyFn = do
  DaoOriginateData{..} <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender (AddressResolved dodOwner1) $
    call dodDao (Call @"Freeze") (#amount .! 10)
  -- Check the token contract got a transfer call from
  -- baseDAO
  checkStorage (AddressResolved $ unTAddress dodTokenContract)
    (toVal [[FA2.TransferItem { tiFrom = dodOwner1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }] }]])

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  withSender (AddressResolved dodOwner1) $ call dodDao (Call @"Propose") params
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Check total supply
  totalSupply <- getTotalSupplyFn (AddressResolved $ unTAddress dodDao ) frozenTokenId
  totalSupply @== 210 -- initial = 0

rejectProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
rejectProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender (AddressResolved dodOwner1) $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  withSender (AddressResolved dodOwner1) $ call dodDao (Call @"Propose") params
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dodDao

nonUniqueProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonUniqueProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender (AddressResolved dodOwner1) $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)
  _ <- createSampleProposal 1 dodOwner1 dodDao
  createSampleProposal 1 dodOwner1 dodDao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE dodDao

voteValidProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteValidProposal originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig

  withSender (AddressResolved dodOwner2) $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender (AddressResolved dodOwner1) $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceTime (sec 10)

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceTime (sec 10)
  withSender (AddressResolved dodOwner2) $ call dodDao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dodDao dodOwner2 102
  -- TODO [#31]: check if the vote is updated properly
