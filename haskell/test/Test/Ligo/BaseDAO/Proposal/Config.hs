-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Ligo.BaseDAO.Proposal.Config
  ( IsConfigDescExt (..)

  , RejectedProposalSlashValue (..)
  , DecisionLambdaAction (..)

  , testConfig
  , badRejectedValueConfig
  , decisionLambdaConfig
  , voteConfig
  ) where

import Lorentz
import Universum (fromIntegral)

import Morley.Util.Named

import qualified Ligo.BaseDAO.Types as DAO
import Test.Ligo.BaseDAO.Common.Errors (tooSmallXtzErrMsg)

-- | Configuration descriptor.
--
-- Implementing conversion of Lorentz config to Ligo config or backwards
-- would be non-trivial and take a lot of effort (though probably is doable),
-- so we can't do this for now.
-- Instead, we introduce config descriptors that compile both to Lorentz and Ligo
-- configs.
--
-- They can be useful outside of the tests, e.g. there are several commonly useful
-- implementations of 'cProposalCheck' and we can add helpers represented as instances
-- of this class.
class IsConfigDescExt configDesc where
  -- | Fill a portion of config specified by descriptor.
  --
  -- For purposes of composing multiple descriptors we implement it as
  -- config modifier.
  (>>-) :: configDesc -> DAO.Config -> DAO.Config

infixr 5 >>-
-- Config descriptors
------------------------------------------------------------------------

data ProposalFrozenTokensCheck =
  ProposalFrozenTokensCheck (Lambda ("ppFrozenToken" :! Natural) ())

data RejectedProposalSlashValue =
  RejectedProposalSlashValue (Lambda ("proposerFrozenToken" :! Natural) ("slash_amount" :! Natural))

proposalFrozenTokensMinBound :: Natural -> ProposalFrozenTokensCheck
proposalFrozenTokensMinBound minTokens = ProposalFrozenTokensCheck $ do
  push minTokens
  toNamed #requireValue
  if #requireValue <=. #ppFrozenToken then
    push ()
  else do
    push tooSmallXtzErrMsg
    failCustom #fAIL_PROPOSAL_CHECK

divideOnRejectionBy :: Natural -> RejectedProposalSlashValue
divideOnRejectionBy divisor = RejectedProposalSlashValue $ do
  fromNamed #proposerFrozenToken
  push divisor
  swap
  ediv
  ifSome car $
    push (0 :: Natural)
  toNamed #slash_amount

doNonsenseOnRejection :: RejectedProposalSlashValue
doNonsenseOnRejection = RejectedProposalSlashValue $ do
  drop; push (10 :: Natural)
  toNamed #slash_amount

data DecisionLambdaAction =
  DecisionLambdaAction
  (["frozen_tokens" :! Natural, "proposer" :! Address] :-> '[[Operation]])

-- | Pass frozen tokens amount as argument to the given contract.
passProposerOnDecision
  :: TAddress ("proposer" :! Address) -> DecisionLambdaAction
passProposerOnDecision target = DecisionLambdaAction $ do
  drop @("frozen_tokens" :! _)
  dip @("proposer" :! _) $ do
    push target
    contract; assertSome [mt|Cannot find contract for decision lambda|]
    push zeroMutez
  transferTokens
  dip nil; cons

-- Config samples
------------------------------------------------------------------------

testConfig
  :: DAO.Config -> DAO.Config
testConfig configIn =
  (divideOnRejectionBy 2) >>-
  (proposalFrozenTokensMinBound 10) >>- configIn

-- | Config with longer voting period and bigger quorum threshold
-- Needed for vote related tests that do not call `flush`
voteConfig
  :: DAO.Config -> DAO.Config
voteConfig configIn =
  (proposalFrozenTokensMinBound 10) >>- configIn

badRejectedValueConfig
  :: RejectedProposalSlashValue
badRejectedValueConfig = doNonsenseOnRejection

decisionLambdaConfig
  :: TAddress ("proposer" :! Address) -> DecisionLambdaAction
decisionLambdaConfig target = passProposerOnDecision target

--------------------------------------------------------------------------------
instance IsConfigDescExt DAO.Period where
  vp >>- DAO.Config{..} = DAO.Config
    { cPeriod = vp
    , ..
    }

instance IsConfigDescExt DAO.FlushLevel where
  (DAO.FlushLevel vp) >>- DAO.Config{..} = DAO.Config
    { cProposalFlushLevel = vp
    , ..
    }

instance IsConfigDescExt DAO.MaxVoters where
  (DAO.MaxVoters vp) >>- DAO.Config{..} = DAO.Config
    { cMaxVoters = vp
    , ..
    }

instance IsConfigDescExt DAO.ExpireLevel where
  (DAO.ExpireLevel vp) >>- DAO.Config{..} = DAO.Config
    { cProposalExpiredLevel = vp
    , ..
    }

instance IsConfigDescExt DAO.MaxProposals where
  (DAO.MaxProposals vp) >>- DAO.Config{..} = DAO.Config
    { cMaxProposals = vp
    , ..
    }

instance IsConfigDescExt DAO.FixedFee where
  ff >>- DAO.Config{..} = DAO.Config
    { cFixedProposalFee = ff
    , ..
    }

instance IsConfigDescExt ProposalFrozenTokensCheck where
  (ProposalFrozenTokensCheck check) >>- DAO.Config{..} = DAO.Config
    { cProposalCheck = do
        dip drop
        toFieldNamed #ppFrozenToken
        framed check
    , ..
    }

instance IsConfigDescExt RejectedProposalSlashValue where
  (RejectedProposalSlashValue toSlashValue) >>- DAO.Config{..} =
    DAO.Config
    { cRejectedProposalSlashValue = do
        dip drop
        toField #plProposerFrozenToken; toNamed #proposerFrozenToken
        framed toSlashValue
    , ..
    }

instance IsConfigDescExt DecisionLambdaAction where
  (DecisionLambdaAction lam) >>- DAO.Config{..} =
    DAO.Config
    { cDecisionLambda = do
        getField #diProposal
        getField #plProposerFrozenToken; toNamed #frozen_tokens
        dip $ do toField #plProposer; toNamed #proposer;
        dip (dip $ do toField #diExtra)
        framed lam
        swap
        dip (push Nothing)
        constructStack @DAO.DecisionLambdaOutput

    , ..
    }

instance IsConfigDescExt ("changePercent" :! Natural) where
  (N cp) >>- DAO.Config{..} =
    DAO.Config
    { cQuorumChange = DAO.QuorumFraction $ fromIntegral $ DAO.percentageToFractionNumerator cp
    , ..
    }

instance IsConfigDescExt ("maxChangePercent" :! Natural) where
  (N cp) >>- DAO.Config{..} =
    DAO.Config
    { cMaxQuorumChange = DAO.QuorumFraction $ fromIntegral $ DAO.percentageToFractionNumerator cp
    , ..
    }

instance IsConfigDescExt ("governanceTotalSupply" :! Natural) where
  (N ts) >>- DAO.Config{..} =
    DAO.Config
    { cGovernanceTotalSupply = DAO.GovernanceTotalSupply ts
    , ..
    }
