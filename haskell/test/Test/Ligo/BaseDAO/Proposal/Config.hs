-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Ligo.BaseDAO.Proposal.Config
  ( IsConfigDescExt (..)
  , ConfigDesc (..)
  , (>>-)

  , ConfigConstants (..)
  , configConsts

  , testConfig
  , voteConfig
  ) where

import qualified Data.Map as Map

import Lorentz
import Universum (Constraint, fromIntegral, (?:))

import Morley.Util.Named
import Morley.Michelson.Typed.Haskell.Value (BigMap(..))

import qualified Ligo.BaseDAO.Types as DAO

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
class IsConfigDescExt config configDesc where
  -- | Fill a portion of config specified by descriptor.
  --
  -- For purposes of composing multiple descriptors we implement it as
  -- config modifier.
  fillConfig :: configDesc -> config -> config

instance IsConfigDescExt c () where
  fillConfig () c = c

-- | Require many config descriptors to have 'IsConfigDescExt' instance.
type family AreConfigDescsExt config descs :: Constraint where
  AreConfigDescsExt _ '[] = ()
  AreConfigDescsExt config (d ': ds) =
    (IsConfigDescExt config d, AreConfigDescsExt config ds)

-- | Some config descriptor.
data ConfigDesc config =
  forall configDesc. IsConfigDescExt config configDesc => ConfigDesc configDesc

instance (config ~ config1) =>
  IsConfigDescExt config (ConfigDesc config1) where
  fillConfig (ConfigDesc d) = fillConfig d

-- | Chaining capability.
data ConfigDescChain a b = ConfigDescChain a b

instance (IsConfigDescExt c a, IsConfigDescExt c b) =>
         IsConfigDescExt c (ConfigDescChain a b) where
  fillConfig (ConfigDescChain a b) = fillConfig b . fillConfig a

-- | Chains two descriptors.
-- In case they modify the same fields, the right-most takes priority.
infixr 5 >>-
(>>-) :: ConfigDesc c -> ConfigDesc c -> ConfigDesc c
ConfigDesc a >>- ConfigDesc b = ConfigDesc (ConfigDescChain a b)


-- Config descriptors
------------------------------------------------------------------------

data ConfigConstants = ConfigConstants
  { cmMaxVoters :: Maybe Natural
  , cmQuorumThreshold :: Maybe DAO.QuorumThreshold
  , cmPeriod :: Maybe DAO.Period
  , cmProposalFlushTime :: Maybe Natural
  , cmProposalExpiredTime :: Maybe Natural
  }

-- | Constructor for config descriptor that overrides config constants.
--
-- Example: @configConsts{ cmMaxVotes = 10 }@
configConsts :: ConfigConstants
configConsts = ConfigConstants Nothing Nothing Nothing Nothing Nothing

data ProposalFrozenTokensCheck =
  ProposalFrozenTokensCheck (Lambda (DAO.ProposeParams, DAO.ContractExtra) ())

data RejectedProposalSlashValue =
  RejectedProposalSlashValue (Lambda (DAO.Proposal, DAO.ContractExtra) Natural)

data DecisionCallbackAction =
  DecisionCallbackAction
  ('[DAO.DecisionCallbackInput] :-> '[DAO.DecisionCallbackOutput])

-- Config samples
------------------------------------------------------------------------

testConfig
  :: AreConfigDescsExt config '[ConfigConstants]
  => ConfigDesc config
testConfig =
  ConfigDesc configConsts
    { cmQuorumThreshold = Just (DAO.mkQuorumThreshold 1 100) }

-- | Config with longer voting period and bigger quorum threshold
-- Needed for vote related tests that do not call `flush`
voteConfig
  :: AreConfigDescsExt config '[ConfigConstants]
  => ConfigDesc config
voteConfig = ConfigDesc $
  ConfigDesc configConsts
    { cmQuorumThreshold = Just (DAO.mkQuorumThreshold 4 100) }

--------------------------------------------------------------------------------
--
instance IsConfigDescExt DAO.Config ConfigConstants where
  fillConfig ConfigConstants{..} DAO.Config{..} = DAO.Config
    { cProposalFlushLevel = cmProposalFlushTime ?: cProposalFlushLevel
    , cProposalExpiredLevel = cmProposalExpiredTime ?: cProposalExpiredLevel
    , ..
    }

instance IsConfigDescExt DAO.Config DAO.Period where
  fillConfig vp DAO.Config{..} = DAO.Config
    { cPeriod = vp
    , ..
    }

instance IsConfigDescExt DAO.Config DAO.FixedFee where
  fillConfig ff DAO.Config{..} = DAO.Config
    { cFixedProposalFee = ff
    , ..
    }

instance IsConfigDescExt DAO.ContractExtra ProposalFrozenTokensCheck where
  fillConfig (ProposalFrozenTokensCheck check) c = DAO.DynamicRec' $
    mkBigMap $ Map.insert [mt|proposal_check|] (lPackValueRaw check) $ bmMap $ DAO.unDynamic c

instance IsConfigDescExt DAO.ContractExtra RejectedProposalSlashValue where
  fillConfig (RejectedProposalSlashValue toSlashValue) c = DAO.DynamicRec' $
    BigMap Nothing $ Map.insert [mt|rejected_proposal_slash_value|] (lPackValueRaw toSlashValue) $ bmMap $ DAO.unDynamic c

instance IsConfigDescExt DAO.ContractExtra DecisionCallbackAction where
  fillConfig (DecisionCallbackAction lam) c = DAO.DynamicRec' $
    BigMap Nothing $ Map.insert [mt|decision_callback|] (lPackValueRaw lam) $ bmMap $ DAO.unDynamic c

instance IsConfigDescExt DAO.Config ("changePercent" :! Natural) where
  fillConfig (N cp) DAO.Config{..} =
    DAO.Config
    { cQuorumChange = DAO.QuorumFraction $ fromIntegral $ DAO.percentageToFractionNumerator cp
    , ..
    }

instance IsConfigDescExt DAO.Config ("maxChangePercent" :! Natural) where
  fillConfig (N cp) DAO.Config{..} =
    DAO.Config
    { cMaxQuorumChange = DAO.QuorumFraction $ fromIntegral $ DAO.percentageToFractionNumerator cp
    , ..
    }

instance IsConfigDescExt DAO.Config ("governanceTotalSupply" :! Natural) where
  fillConfig (N ts) DAO.Config{..} =
    DAO.Config
    { cGovernanceTotalSupply = DAO.GovernanceTotalSupply ts
    , ..
    }
