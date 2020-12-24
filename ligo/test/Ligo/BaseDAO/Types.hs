-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE FunctionalDependencies #-}

-- | Types mirrored from LIGO implementation.
module Ligo.BaseDAO.Types
  ( module Lorentz.Contracts.BaseDAO.Types
  , ProposalMetadataL
  , ContractExtraL
  , ProposeParamsL
  , ProposalL (..)
  , VoteParamL
  , Parameter (..)
  , Storage (..)
  , Config (..)
  , FullStorage (..)
  ) where

import Lorentz
import Universum (One, Show)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16

import Lorentz.Contracts.BaseDAO.Types hiding (Config(..), Parameter(..), Storage(..))
import Michelson.Typed.Haskell.Compatibility

-- | Represents a product type with arbitrary fields.
--
-- Contains a name to make different such records distinguishable.
newtype DynamicRec n = DynamicRec { unDynamic :: Map MText ByteString }
  deriving stock (Generic, Show, Eq)
  deriving newtype (IsoValue, HasAnnotation, Default, One, Semigroup, Monoid)

type ProposalMetadataL = DynamicRec "pm"
type ContractExtraL = DynamicRec "ce"
type CustomEntrypoints = DynamicRec "ep"

type ProposeParamsL = ProposeParams ProposalMetadataL
type ProposalKeyL = ProposalKey ProposalMetadataL
type VoteParamL = VoteParam ProposalMetadataL

data ProposalL = ProposalL
  { plUpvotes             :: Natural
  , plDownvotes           :: Natural
  , plStartDate           :: Timestamp

  , plMetadata            :: ProposalMetadataL

  , plProposer            :: Address
  , plProposerFrozenToken :: Natural

  , plVoters              :: [(Address, Natural)]
  }
  deriving stock (Show)

type CallCustomParam = (MText, ByteString)

data Parameter
  = Accept_ownership ()
  | Burn BurnParam
  | CallCustom CallCustomParam
  | Call_FA2 FA2.Parameter
  | Confirm_migration ()
  | Drop_proposal ProposalKeyL
  | Flush Natural
  | GetVotePermitCounter (View () Nonce)
  | Migrate MigrateParam
  | Mint MintParam
  | Propose ProposeParamsL
  | Set_quorum_threshold QuorumThreshold
  | Set_voting_period VotingPeriod
  | Transfer_contract_tokens TransferContractTokensParam
  | Transfer_ownership TransferOwnershipParam
  | Vote [PermitProtected VoteParamL]
  deriving stock (Show)

data Storage = Storage
  { sAdmin :: Address
  , sExtra :: ContractExtraL
  , sLedger :: Ledger
  , sMetadata :: "metadata" :! TZIP16.MetadataMap BigMap
  , sMigrationStatus :: MigrationStatus
  , sOperators :: Operators
  , sPendingOwner :: Address
  , sPermitsCounter :: Nonce
  , sProposals :: BigMap ProposalKeyL ProposalL
  , sProposalKeyListSortByDate :: Set (Timestamp, ProposalKeyL)
  , sQuorumThreshold :: QuorumThreshold
  , sTokenAddress :: Address
  , sVotingPeriod :: VotingPeriod
  }
  deriving stock (Show)

data Config = Config
  { cUnfrozenTokenMetadata :: FA2.TokenMetadata
  , cFrozenTokenMetadata :: FA2.TokenMetadata
  , cProposalCheck :: '[ProposeParamsL, Storage] :-> '[Bool]
  , cRejectedProposalReturnValue :: '[ProposalL, Storage] :-> '["slash_amount" :! Natural]
  , cDecisionLambda :: '[ProposalL, Storage] :-> '[List Operation, Storage]

  , cMaxProposals :: Natural
  , cMaxVotes :: Natural
  , cMaxQuorumThreshold :: Natural
  , cMinQuorumThreshold :: Natural

  , cMaxVotingPeriod :: Natural
  , cMinVotingPeriod :: Natural

  , cCustomEntrypoints :: CustomEntrypoints
  }
  deriving stock (Show)

data FullStorage = FullStorage
  { fsStorage :: Storage
  , fsConfig :: Config
  }
  deriving stock (Show)

-- Instances
------------------------------------------------

customGeneric "ProposalL" ligoLayout
deriving anyclass instance IsoValue ProposalL

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate

customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage

customGeneric "Config" ligoLayout
deriving anyclass instance IsoValue Config

deriving stock instance Generic FullStorage
deriving anyclass instance IsoValue FullStorage
