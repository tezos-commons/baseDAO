-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Types mirrored from LIGO implementation.
module Ligo.BaseDAO.Types
  ( module Lorentz.Contracts.BaseDAO.Types
  , ProposalMetadataL
  , ContractExtraL
  , ProposeParamsL
  , CustomEntrypointsL
  , ProposalL (..)
  , VoteParamL
  , ParameterL (..)
  , StorageL (..)
  , ConfigL (..)
  , FullStorage (..)
  , DynamicRec (..)
  ) where

import Lorentz
import Universum (One(..), Show, fromIntegral)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16

import BaseDAO.ShareTest.Common (ProposalMetadataFromNum(..))
import Lorentz.Contracts.BaseDAO.Types hiding
  (Config(..), Parameter(..), Storage(..), defaultConfig)
import Michelson.Typed.Haskell.Compatibility

-- | Represents a product type with arbitrary fields.
--
-- Contains a name to make different such records distinguishable.
newtype DynamicRec n = DynamicRec { unDynamic :: Map MText ByteString }
  deriving stock (Generic, Show, Eq)
  deriving newtype (IsoValue, HasAnnotation, Default, One, Semigroup, Monoid)

type ProposalMetadataL = DynamicRec "pm"
type ContractExtraL = DynamicRec "ce"
type CustomEntrypointsL = DynamicRec "ep"

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

data ParameterL
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

data StorageL = StorageL
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

data ConfigL = ConfigL
  { cUnfrozenTokenMetadata :: FA2.TokenMetadata
  , cFrozenTokenMetadata :: FA2.TokenMetadata
  , cProposalCheck :: '[ProposeParamsL, StorageL] :-> '[Bool]
  , cRejectedProposalReturnValue :: '[ProposalL, StorageL] :-> '["slash_amount" :! Natural]
  , cDecisionLambda :: '[ProposalL, StorageL] :-> '[List Operation, StorageL]

  , cMaxProposals :: Natural
  , cMaxVotes :: Natural
  , cMaxQuorumThreshold :: Natural
  , cMinQuorumThreshold :: Natural

  , cMaxVotingPeriod :: Natural
  , cMinVotingPeriod :: Natural

  , cCustomEntrypoints :: CustomEntrypointsL
  }
  deriving stock (Show)

data FullStorage = FullStorage
  { fsStorage :: StorageL
  , fsConfig :: ConfigL
  }
  deriving stock (Show)

-- Instances
------------------------------------------------

customGeneric "ProposalL" ligoLayout
deriving anyclass instance IsoValue ProposalL

customGeneric "ParameterL" ligoLayout
deriving anyclass instance IsoValue ParameterL
instance ParameterHasEntrypoints ParameterL where
  type ParameterEntrypointsDerivation ParameterL = EpdDelegate

customGeneric "StorageL" ligoLayout
deriving anyclass instance IsoValue StorageL

customGeneric "ConfigL" ligoLayout
deriving anyclass instance IsoValue ConfigL

deriving stock instance Generic FullStorage
deriving anyclass instance IsoValue FullStorage

instance ProposalMetadataFromNum ProposalMetadataL where
  proposalMetadataFromNum n =
    one ([mt|int|], lPackValueRaw @Integer $ fromIntegral n)
