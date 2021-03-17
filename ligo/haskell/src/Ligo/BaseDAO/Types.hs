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
  , ParameterL
  , StorageL (..)
  , ConfigL (..)
  , FullStorage (..)
  , AddressFreezeHistory (..)
  , LastPeriodChange (..)
  , DynamicRec (..)
  , dynRecUnsafe
  , mkStorageL
  , mkMetadataMap
  , mkConfigL
  , defaultConfigL
  , mkFullStorageL

  , sOperatorsLens
  ) where

import Lorentz hiding (now)
import Universum (One(..), fromIntegral, maybe, (*))

import Control.Lens (makeLensesFor)
import qualified Data.Map as M

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Lorentz.Annotation ()

import Michelson.Runtime.GState (genesisAddress)
import Lorentz.Contracts.BaseDAO.Types hiding
  (Config(..), Parameter(..), Storage(..), defaultConfig)

import BaseDAO.ShareTest.Common (ProposalMetadataFromNum(..))

-- | Represents a product type with arbitrary fields.
--
-- Contains a name to make different such records distinguishable.
newtype DynamicRec n = DynamicRec { unDynamic :: Map MText ByteString }
  deriving stock (Generic, Show, Eq)
  deriving newtype (IsoValue, HasAnnotation, Default, One, Semigroup)

-- | Construct 'DynamicRec' assuming it contains no mandatory entries.
dynRecUnsafe :: DynamicRec n
dynRecUnsafe = DynamicRec mempty

-- TODO consider making these all 'BigMap's instead
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
  , plProposerFixedFeeInToken :: Natural

  , plVoters              :: [Voter]
  }
  deriving stock (Show)

-- | Utility type containing an entrypoint name and its packed argument.
type CallCustomParam = (MText, ByteString)

-- | Utility type containing an entrypoint name and its packed lambda.
type CustomEntrypoint = (MText, ByteString)

type FreezeParam = ("amount" :! Natural)
type UnfreezeParam = ("amount" :! Natural)

-- NOTE: Constructors of the parameter types should remain sorted for the
--'ligoLayout' custom derivation to work.
--
-- TODO: This above will be no longer the case once the following issue is fixed.
-- https://gitlab.com/morley-framework/morley/-/issues/527
data ForbidXTZParam
  = Accept_ownership ()
  | Burn BurnParam
  | Call_FA2 FA2.Parameter
  | Confirm_migration ()
  | Drop_proposal ProposalKeyL
  | Flush Natural
  | Freeze FreezeParam
  | GetVotePermitCounter (View () Nonce)
  | Get_total_supply (View FA2.TokenId Natural)
  | Migrate MigrateParam
  | Mint MintParam
  | Set_fixed_fee_in_token Natural
  | Set_quorum_threshold QuorumThreshold
  | Set_voting_period VotingPeriod
  | Transfer_ownership TransferOwnershipParam
  | Unfreeze UnfreezeParam
  | Vote [PermitProtected VoteParamL]
  deriving stock (Show)

data AllowXTZParam
  = CallCustom CallCustomParam
  | Propose ProposeParamsL
  deriving stock (Show)

data MigratableParam
  = XtzAllowed AllowXTZParam
  | XtzForbidden ForbidXTZParam
  deriving stock (Show)

data ParameterL
  = Migratable MigratableParam
  | Transfer_contract_tokens TransferContractTokensParam
  deriving stock (Show)

data LastPeriodChange = LastPeriodChange
  { vhPeriodNum :: Natural
  , vhChangedOn :: Timestamp
  } deriving stock (Eq, Show)

data AddressFreezeHistory = AddressFreezeHistory
  { fhCurrentUnstaked :: Natural
  , fhPastUnstaked :: Natural
  , fhCurrentPeriodNum :: Natural
  , fhStaked :: Natural
  } deriving stock (Eq, Show)

data StorageL = StorageL
  { sAdmin :: Address
  , sExtra :: ContractExtraL
  , sFrozenTokenId :: FA2.TokenId
  , sLedger :: Ledger
  , sMetadata :: TZIP16.MetadataMap BigMap
  , sMigrationStatus :: MigrationStatus
  , sOperators :: Operators
  , sPendingOwner :: Address
  , sPermitsCounter :: Nonce
  , sProposals :: BigMap ProposalKeyL ProposalL
  , sProposalKeyListSortByDate :: Set (Timestamp, ProposalKeyL)
  , sQuorumThreshold :: QuorumThreshold
  , sTokenAddress :: Address
  , sVotingPeriod :: VotingPeriod
  , sTotalSupply :: TotalSupply
  , sUnfrozenTokenId :: FA2.TokenId
  , sFreezeHistory :: BigMap Address AddressFreezeHistory
  , sLastPeriodChange :: LastPeriodChange
  , sFixedProposalFeeInToken :: Natural
  }
  deriving stock (Show)

instance HasAnnotation ProposalL where
  annOptions = baseDaoAnnOptions

instance HasAnnotation StorageL where
  annOptions = baseDaoAnnOptions

instance HasAnnotation AddressFreezeHistory where
  annOptions = baseDaoAnnOptions

instance HasAnnotation FullStorage where
  annOptions = baseDaoAnnOptions

instance HasAnnotation ConfigL where
  annOptions = baseDaoAnnOptions

instance HasAnnotation LastPeriodChange where
  annOptions = baseDaoAnnOptions

instance HasFieldOfType StorageL name field =>
         StoreHasField StorageL name field where
  storeFieldOps = storeFieldOpsADT

instance StoreHasSubmap StorageL "sLedger" LedgerKey LedgerValue where
  storeSubmapOps = storeSubmapOpsDeeper #sLedger

instance StoreHasSubmap StorageL "sOperators" ("owner" :! Address, "operator" :! Address) () where
  storeSubmapOps = storeSubmapOpsDeeper #sOperators

instance StoreHasSubmap StorageL "sTotalSupply" FA2.TokenId Natural where
  storeSubmapOps = storeSubmapOpsDeeper #sTotalSupply

mkStorageL
  :: "admin" :! Address
  -> "votingPeriod" :? Natural
  -> "quorumThreshold" :? Natural
  -> "extra" :! ContractExtraL
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> "now" :! Timestamp
  -> StorageL
mkStorageL admin votingPeriod quorumThreshold extra metadata now =
  StorageL
    { sAdmin = arg #admin admin
    , sExtra = arg #extra extra
    , sLedger = mempty
    , sMetadata = arg #metadata metadata
    , sMigrationStatus = NotInMigration
    , sOperators = mempty
    , sPendingOwner = arg #admin admin
    , sPermitsCounter = Nonce 0
    , sProposals = mempty
    , sProposalKeyListSortByDate = mempty
    , sQuorumThreshold = argDef #quorumThreshold quorumThresholdDef quorumThreshold
    , sTokenAddress = genesisAddress
    , sVotingPeriod = argDef #votingPeriod votingPeriodDef votingPeriod
    , sTotalSupply = M.fromList [(frozenTokenId, 0), (unfrozenTokenId, 0)]
    , sFreezeHistory = mempty
    , sLastPeriodChange = LastPeriodChange 0 (arg #now now)
    , sFixedProposalFeeInToken = 0
    , sUnfrozenTokenId = unfrozenTokenId
    , sFrozenTokenId = frozenTokenId
    }
  where
    votingPeriodDef = 60 * 60 * 24 * 7  -- 7 days
    quorumThresholdDef = 4

mkMetadataMap
  :: "metadataHostAddress" :! Address
  -> "metadataHostChain" :? TZIP16.ExtChainId
  -> "metadataKey" :! MText
  -> TZIP16.MetadataMap BigMap
mkMetadataMap hostAddress hostChain key =
  TZIP16.metadataURI . TZIP16.tezosStorageUri host $ arg #metadataKey key
  where
    host = maybe
      TZIP16.contractHost
      TZIP16.foreignContractHost
      (argF #metadataHostChain hostChain)
      (arg #metadataHostAddress hostAddress)

data ConfigL = ConfigL
  { cProposalCheck :: '[ProposeParamsL, ContractExtraL] :-> '[Bool]
  , cRejectedProposalReturnValue :: '[ProposalL, ContractExtraL] :-> '["slash_amount" :! Natural]
  , cDecisionLambda :: '[ProposalL, ContractExtraL] :-> '[List Operation, ContractExtraL]

  , cMaxProposals :: Natural
  , cMaxVotes :: Natural
  , cMaxQuorumThreshold :: Natural
  , cMinQuorumThreshold :: Natural

  , cMaxVotingPeriod :: Natural
  , cMinVotingPeriod :: Natural

  , cCustomEntrypoints :: CustomEntrypointsL
  }
  deriving stock (Show)

mkConfigL :: [CustomEntrypoint] -> ConfigL
mkConfigL customEps = ConfigL
  { cProposalCheck = do
      dropN @2; push True
  , cRejectedProposalReturnValue = do
      dropN @2; push (0 :: Natural); toNamed #slash_amount
  , cDecisionLambda = do
      drop; nil
  , cCustomEntrypoints = DynamicRec $ M.fromList customEps

  , cMaxVotingPeriod = 60 * 60 * 24 * 30
  , cMinVotingPeriod = 1

  , cMaxQuorumThreshold = 1000
  , cMinQuorumThreshold = 1

  , cMaxVotes = 1000
  , cMaxProposals = 500
  }

defaultConfigL :: ConfigL
defaultConfigL = mkConfigL []

data FullStorage = FullStorage
  { fsStorage :: StorageL
  , fsConfig :: ConfigL
  }
  deriving stock (Show)

mkFullStorageL
  :: "admin" :! Address
  -> "votingPeriod" :? Natural
  -> "quorumThreshold" :? Natural
  -> "extra" :! ContractExtraL
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> "now" :! Timestamp
  -> "customEps" :? [CustomEntrypoint]
  -> FullStorage
mkFullStorageL admin vp qt extra md now cEps = FullStorage
  { fsStorage = mkStorageL admin vp qt extra md now
  , fsConfig  = mkConfigL (argDef #customEps [] cEps)
  }

-- Instances
------------------------------------------------

customGeneric "ProposalL" ligoLayout
deriving anyclass instance IsoValue ProposalL

customGeneric "MigratableParam" ligoLayout
deriving anyclass instance IsoValue MigratableParam
instance ParameterHasEntrypoints MigratableParam where
  type ParameterEntrypointsDerivation MigratableParam = EpdDelegate

customGeneric "ForbidXTZParam" ligoLayout
deriving anyclass instance IsoValue ForbidXTZParam
instance ParameterHasEntrypoints ForbidXTZParam where
  type ParameterEntrypointsDerivation ForbidXTZParam = EpdDelegate

customGeneric "AllowXTZParam" ligoLayout
deriving anyclass instance IsoValue AllowXTZParam
instance ParameterHasEntrypoints AllowXTZParam where
  type ParameterEntrypointsDerivation AllowXTZParam = EpdDelegate

customGeneric "ParameterL" ligoLayout
deriving anyclass instance IsoValue ParameterL
instance ParameterHasEntrypoints ParameterL where
  type ParameterEntrypointsDerivation ParameterL = EpdDelegate

customGeneric "AddressFreezeHistory" ligoLayout
deriving anyclass instance IsoValue AddressFreezeHistory

customGeneric "LastPeriodChange" ligoLayout
deriving anyclass instance IsoValue LastPeriodChange

customGeneric "StorageL" ligoLayout
deriving anyclass instance IsoValue StorageL

customGeneric "ConfigL" ligoLayout
deriving anyclass instance IsoValue ConfigL

deriving stock instance Generic FullStorage
deriving anyclass instance IsoValue FullStorage

instance ProposalMetadataFromNum ProposalMetadataL where
  proposalMetadataFromNum n =
    one ([mt|int|], lPackValueRaw @Integer $ fromIntegral n)

-- Lenses
------------------------------------------------

makeLensesFor
  [ ("sOperators", "sOperatorsLens")
  ] ''StorageL
