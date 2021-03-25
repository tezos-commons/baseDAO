-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Specification of config descriptors on LIGO contract.
module Test.Ligo.BaseDAO.ConfigDesc
  ( module Test.Ligo.BaseDAO.Proposal.Config
  ) where

import Lorentz
import Universum ((?:))

import Test.Ligo.BaseDAO.Proposal.Config
import Ligo.BaseDAO.Types

instance IsConfigDescExt ConfigL ConfigConstants where
  fillConfig ConfigConstants{..} ConfigL{..} = ConfigL
    { cMaxProposals = cmMaxProposals ?: cMaxProposals
    , cMaxVotes = cmMaxVotes ?: cMaxVotes
    , cMinVotingPeriod = cmMinVotingPeriod ?: cMinVotingPeriod
    , cMaxVotingPeriod = cmMaxVotingPeriod ?: cMaxVotingPeriod
    , cMinQuorumThreshold = cmMinQuorumThreshold ?: cMinQuorumThreshold
    , cMaxQuorumThreshold = cmMaxQuorumThreshold ?: cMaxQuorumThreshold
    , ..
    }

instance IsConfigDescExt ConfigL ProposalFrozenTokensCheck where
  fillConfig (ProposalFrozenTokensCheck check) ConfigL{..} = ConfigL
    { cProposalCheck = do
        dip drop
        toFieldNamed #ppFrozenToken
        framed check
    , ..
    }

instance IsConfigDescExt ConfigL RejectedProposalReturnValue where
  fillConfig (RejectedProposalReturnValue toReturnValue) ConfigL{..} =
    ConfigL
    { cRejectedProposalReturnValue = do
        dip drop
        toField #plProposerFrozenToken; toNamed #proposerFrozenToken
        framed toReturnValue
    , ..
    }

instance (pm ~ ProposalMetadataL) => IsConfigDescExt ConfigL DecisionLambdaAction where
  fillConfig (DecisionLambdaAction lam) ConfigL{..} =
    ConfigL
    { cDecisionLambda = do
        getField #plProposerFrozenToken; toNamed #frozen_tokens
        dip $ do toField #plProposer; toNamed #proposer
        framed lam
    , ..
    }
