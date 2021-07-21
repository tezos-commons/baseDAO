-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types mirrored from LIGO implementation.
module Ligo.BaseDAO.Types
  ( frozenTokenId
  , baseDaoAnnOptions

    -- * Proposals
  , DecisionLambdaInput (..)
  , DecisionLambdaOutput (..)
  , ProposalKey
  , ProposeParams(..)
  , GovernanceToken(..)

    -- * Voting
  , QuorumThreshold (..)
  , QuorumThresholdAtCycle (..)
  , Period (..)
  , VoteParam (..)
  , Voter (..)
  , QuorumFraction (..)
  , GovernanceTotalSupply (..)
  , mkQuorumThreshold
  , fractionDenominator
  , percentageToFractionNumerator

  -- * Delegates
  , Delegate (..)
  , Delegates
  , DelegateParam (..)

    -- * Non FA2
  , TransferContractTokensParam (..)

    -- * Permissions
  , Nonce (..)
  , DataToSign (..)
  , Permit (..)
  , PermitProtected (..)
  , pattern NoPermit

    -- * Storage/Parameter
  , TransferOwnershipParam
  , ProposalMetadata
  , ContractExtra
  , CustomEntrypoints
  , CallCustomParam
  , Proposal (..)
  , Parameter (..)
  , ForbidXTZParam (..)
  , AllowXTZParam (..)
  , FreezeParam
  , UnfreezeParam

  , FixedFee (..)
  , Storage' (..)
  , Storage
  , StorageView
  , Config' (..)
  , Config
  , FullStorage' (..)
  , FullStorage
  , FullStorageView
  , AddressFreezeHistory (..)
  , DynamicRec
  , DynamicRec' (..)
  , dynRecUnsafe
  , mkStorage
  , mkMetadataMap
  , mkConfig
  , defaultConfig
  , mkFullStorage
  , setExtra
  ) where

import Universum (Enum, Integral, Num, One(..), Real, fromIntegral, div, maybe, (*), show)

import Fmt (Buildable, build, genericF)
import qualified Data.Map as M

import Lorentz hiding (now)
import Lorentz.Annotation ()
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Michelson.Typed.Annotation
import Michelson.Typed.T (T(TUnit))
import Michelson.Untyped.Annotation
import Morley.Client (BigMapId(..))
import Util.Markdown

------------------------------------------------------------------------
-- Orphans
------------------------------------------------------------------------

customGeneric "FA2.Parameter" ligoLayout

deriving anyclass instance IsoValue FA2.Parameter

instance ParameterHasEntrypoints FA2.Parameter where
  type ParameterEntrypointsDerivation FA2.Parameter = EpdPlain

instance TypeHasDoc FA2.Parameter where
  typeDocMdDescription = "Describes the FA2 operations."

instance HasAnnotation FA2.Parameter

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

frozenTokenId :: FA2.TokenId
frozenTokenId = FA2.TokenId 0

baseDaoAnnOptions :: AnnOptions
baseDaoAnnOptions = defaultAnnOptions { fieldAnnModifier = dropPrefixThen toSnake }

------------------------------------------------------------------------
-- Proposals
------------------------------------------------------------------------

data ProposeParams = ProposeParams
  { ppFrom             :: Address
  , ppFrozenToken      :: Natural
  --  ^ Determines how many sender's tokens will be frozen to get
  -- the proposal accepted
  , ppProposalMetadata :: ProposalMetadata
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass IsoValue

instance TypeHasDoc ProposalMetadata => TypeHasDoc ProposeParams where
  typeDocMdDescription =
     "Describes the how many proposer's frozen tokens will be frozen and the proposal metadata"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @ProposeParams
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @ProposeParams

instance HasAnnotation ProposeParams where
  annOptions = baseDaoAnnOptions

instance Buildable ProposeParams where
  build = genericF

type ProposalKey = Hash Blake2b $ Packed ProposeParams

------------------------------------------------------------------------
-- Voting
------------------------------------------------------------------------

-- | QuorumThreshold that a proposal need to meet, expressed as a fraction of
-- the frozen tokens total supply.
-- A proposal will be rejected if the quorum_threshold is not met,
-- regardless of upvotes > downvotes
-- A proposal will be accepted only if:
-- (quorum_threshold * total_supply >= upvote + downvote) && (upvote > downvote)
newtype QuorumThreshold = QuorumThreshold
  { qtNumerator :: Natural
  }
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue

fractionDenominator :: Integer
fractionDenominator = 1000000

percentageToFractionNumerator :: Num p => p -> p
percentageToFractionNumerator p = p * (fromInteger $ div fractionDenominator 100)

mkQuorumThreshold :: Integer -> Integer -> QuorumThreshold
mkQuorumThreshold n d = QuorumThreshold $ fromInteger $ div (n * fractionDenominator) d

instance HasAnnotation QuorumThreshold where
  annOptions = baseDaoAnnOptions

instance Buildable QuorumThreshold where
  build = genericF

-- | Stages length, in seconds
newtype Period = Period { unPeriod :: Natural}
  deriving stock (Generic, Show, Eq)
  deriving newtype Num
  deriving anyclass IsoValue

instance HasAnnotation Period where
  annOptions = baseDaoAnnOptions
instance Buildable Period where
  build = genericF

newtype QuorumFraction = QuorumFraction Integer
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue

instance HasAnnotation QuorumFraction where
  annOptions = baseDaoAnnOptions
instance Buildable QuorumFraction where
  build = genericF

-- | Voting period in seconds
newtype GovernanceTotalSupply = GovernanceTotalSupply Natural
  deriving stock (Generic, Show, Eq)
  deriving newtype Num
  deriving anyclass IsoValue

instance HasAnnotation GovernanceTotalSupply where
  annOptions = baseDaoAnnOptions
instance Buildable GovernanceTotalSupply where
  build = genericF

-- | Represents whether a voter has voted against (False) or for (True) a given proposal.
type VoteType = Bool

data Voter = Voter
  { voteAmount :: Natural
  , voteType :: VoteType
  }
  deriving stock (Show)

instance TypeHasDoc Voter where
  typeDocMdDescription =
    "Describes a voter on some proposal, including its address, vote type and vote amount"

instance HasAnnotation Voter where
  annOptions = baseDaoAnnOptions

data VoteParam = VoteParam
  { vProposalKey :: ProposalKey
  , vVoteType    :: VoteType
  , vVoteAmount  :: Natural
  , vFrom        :: Address
  }
  deriving stock (Eq, Show)

instance TypeHasDoc ProposalMetadata => TypeHasDoc VoteParam where
  typeDocMdDescription = "Describes target proposal id, vote type and vote amount"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @VoteParam
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @VoteParam

instance HasAnnotation VoteParam where
  annOptions = baseDaoAnnOptions

instance Buildable VoteParam where
  build = genericF

------------------------------------------------------------------------
-- Non FA2
------------------------------------------------------------------------

data TransferContractTokensParam = TransferContractTokensParam
  { tcContractAddress :: Address
  , tcParams          :: FA2.TransferParams
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass IsoValue

instance TypeHasDoc TransferContractTokensParam where
  typeDocMdDescription =
    "Describes an FA2 contract address and the parameter to call its 'transfer' entrypoint"

instance HasAnnotation TransferContractTokensParam where
  annOptions = baseDaoAnnOptions

instance Buildable TransferContractTokensParam where
  build = genericF

------------------------------------------------------------------------
-- Delegates
------------------------------------------------------------------------

data Delegate = Delegate
  { dOwner :: Address
  , dDelegate :: Address
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass IsoValue

instance TypeHasDoc Delegate where
  typeDocMdDescription =
    "Describes a relation of a delegate address and an owner address"

instance HasAnnotation Delegate where
  annOptions = baseDaoAnnOptions

instance Buildable Delegate where
  build = genericF

instance Buildable () where
  build = build . show @Text

type Delegates' big_map = big_map Delegate ()

type Delegates = Delegates' BigMap


data DelegateParam = DelegateParam
  { dpEnable :: Bool
  , dpDelegate :: Address
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass IsoValue

instance TypeHasDoc ProposalMetadata => TypeHasDoc DelegateParam where
  typeDocMdDescription =
     "Describes the parameters to update/remove a delegate."
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @DelegateParam
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @DelegateParam

instance HasAnnotation DelegateParam where
  annOptions = baseDaoAnnOptions

instance Buildable DelegateParam where
  build = genericF

------------------------------------------------------------------------
-- Permissions
------------------------------------------------------------------------

newtype Nonce = Nonce { unNonce :: Natural }
  deriving stock (Generic, Show, Eq)
  deriving newtype (IsoValue, HasAnnotation)

instance Buildable Nonce where
  build = genericF


instance TypeHasDoc Nonce where
  typeDocMdDescription =
    "Contract-local nonce used to make some data unique."

-- | When signing something, you usually want to use this wrapper over your data.
-- It ensures that replay attack is not possible.
data DataToSign d = DataToSign
  { dsChainId :: ChainId
  , dsContract :: Address
  , dsNonce :: Nonce
  , dsData :: d
  } deriving stock (Generic)
    deriving anyclass (IsoValue)

deriving stock instance Eq a => Eq (DataToSign a)

instance HasAnnotation d => HasAnnotation (DataToSign d) where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc d => TypeHasDoc (DataToSign d) where
  typeDocMdDescription = [md|
    A wrapper over data that is to be signed.

    Aside from the original data, this contains elements that ensure the result
    to be globally unique in order to avoid replay attacks:
    * Chain id
    * Address of the contract
    * Nonce - suitable nonce can be fetched using the dedicated endpoint.
    |]
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(DataToSign MText)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(DataToSign MText)

instance CanCastTo a b => DataToSign a `CanCastTo` DataToSign b where
  castDummy = castDummyG

-- | Information about permit.
--
-- Following TZIP-17, this allows a service to execute an entrypoint from
-- user's behalf securely.
--
-- Type argument @a@ stands for argument of entrypoint protected by permit.
data Permit a = Permit
  { pKey :: PublicKey
    -- ^ Key of the user.
  , pSignature :: TSignature $ Packed (DataToSign a)
    -- ^ Parameter signature.
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue)

deriving stock instance Eq a => Eq (TSignature a)
deriving stock instance Eq a => Eq (Permit a)

instance HasAnnotation (Permit a) where
  annOptions = baseDaoAnnOptions

instance Buildable (Permit a) where
  build _ = build @Text "<permit>" -- Unable to set `Buildable` instance for `TSignature`

instance TypeHasDoc a => TypeHasDoc (Permit a) where
  typeDocMdDescription = [md|
    Permission for executing an action from another user's behalf.

    This contains public key of that user and signed argument for the entrypoint.
    Type parameter of `Permit` stands for the entrypoint argument type.
    |]
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(Permit Integer)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Permit Integer)

-- | Parameter, optionally protected with permission.
--
-- If permit is not provided, we treat the current sender as
-- original author of this call.
-- Otherwise we ensure that parameter is indeed signed by the
-- author of the key in permit.
data PermitProtected a = PermitProtected
  { ppArgument :: a
  , ppPermit :: Maybe (Permit a)
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue)

deriving stock instance Eq a => Eq (PermitProtected a)

instance (Buildable a) => Buildable (PermitProtected a) where
  build = genericF

instance HasAnnotation a => HasAnnotation (PermitProtected a) where
  getAnnotation _ =
    NTPair
      [typeAnnQ|permit_protected|]
      (noAnn @FieldTag)
      [fieldAnnQ|permit|]
      noAnn noAnn
      (getAnnotation @a NotFollowEntrypoint)
      (getAnnotation @(Maybe (Permit a)) NotFollowEntrypoint)
    -- TODO: probably it is not assumed to look this way,
    --       rewrite in a prettier way somehow?

-- | Perform operation from sender behalf.
pattern NoPermit :: a -> PermitProtected a
pattern NoPermit a = PermitProtected a Nothing

-- | Type which we know nothing about.
data SomeType

instance TypeHasDoc SomeType where
  typeDocName _ = "SomeType"
  typeDocMdDescription = "Some type, may differ in various situations."
  typeDocDependencies _ = []
  typeDocHaskellRep _ _ = Nothing
  typeDocMichelsonRep _ = (Just "SomeType", TUnit)

instance a `CanCastTo` SomeType

------------------------------------------------------------------------
-- Storage/Parameter
------------------------------------------------------------------------

instance Buildable (ByteString) where
  build = build . show @Text

type TransferOwnershipParam = ("newOwner" :! Address)

-- | Represents a product type with arbitrary fields.
--
-- Contains a name to make different such records distinguishable.
newtype DynamicRec' big_map n = DynamicRec' { unDynamic :: big_map MText ByteString }

type DynamicRec = DynamicRec' BigMap
deriving stock instance Generic (DynamicRec n)
deriving stock instance Show (DynamicRec n)
deriving stock instance Eq (DynamicRec n)
deriving newtype instance IsoValue (DynamicRec n)
deriving newtype instance HasAnnotation (DynamicRec n)
deriving newtype instance Default (DynamicRec n)
deriving newtype instance One (DynamicRec n)
deriving newtype instance Semigroup (DynamicRec n)
instance Buildable (DynamicRec n) where
  build = genericF

type DynamicRecView = DynamicRec' BigMapId
deriving stock instance Generic (DynamicRecView n)
deriving stock instance Show (DynamicRecView n)
deriving newtype instance IsoValue (DynamicRecView n)

-- | Construct 'DynamicRec' assuming it contains no mandatory entries.
dynRecUnsafe :: DynamicRec n
dynRecUnsafe = DynamicRec' mempty

type ProposalMetadata = ByteString

type ContractExtra' big_map = DynamicRec' big_map "ce"
type ContractExtra = ContractExtra' BigMap

type CustomEntrypoints' big_map = DynamicRec' big_map "ep"
type CustomEntrypoints = CustomEntrypoints' BigMap

data Proposal = Proposal
  { plUpvotes                 :: Natural
  , plDownvotes               :: Natural
  , plStartLevel              :: Natural
  , plVotingStageNum          :: Natural

  , plMetadata                :: ProposalMetadata

  , plProposer                :: Address
  , plProposerFrozenToken     :: Natural

  , plVoters                  :: Map (Address, Bool) Natural
  , plQuorumThreshold         :: QuorumThreshold
  }
  deriving stock (Eq, Show)

-- | Utility type containing an entrypoint name and its packed argument.
type CallCustomParam = (MText, ByteString)

-- | Utility type containing an entrypoint name and its packed lambda.
type CustomEntrypoint = (MText, ByteString)

type FreezeParam = ("amount" :! Natural)
type UnfreezeParam = ("amount" :! Natural)

-- NOTE: Constructors of the parameter types should remain sorted for the
--'ligoLayout' custom derivation to work.
--
-- TODO #259: The comment above should no longer be the case since the following
-- issue has been fixed:
-- https://gitlab.com/morley-framework/morley/-/issues/527
data ForbidXTZParam
  = Drop_proposal ProposalKey
  | Flush Natural
  | Freeze FreezeParam
  | Unfreeze UnfreezeParam
  | Update_delegate [DelegateParam]
  | Vote [PermitProtected VoteParam]
  deriving stock (Eq, Show)

instance Buildable ForbidXTZParam where
  build = genericF

data AllowXTZParam
  = CallCustom CallCustomParam
  | Propose ProposeParams
  | Transfer_contract_tokens TransferContractTokensParam
  | Transfer_ownership TransferOwnershipParam
  | Accept_ownership ()
  deriving stock (Eq, Show)

instance Buildable AllowXTZParam where
  build = genericF

data Parameter
  = XtzAllowed AllowXTZParam
  | XtzForbidden ForbidXTZParam
  deriving stock (Eq, Show)

instance Buildable Parameter where
  build = genericF

data AddressFreezeHistory = AddressFreezeHistory
  { fhCurrentUnstaked :: Natural
  , fhPastUnstaked :: Natural
  , fhCurrentStageNum :: Natural
  , fhStaked :: Natural
  } deriving stock (Eq, Show)

instance Buildable AddressFreezeHistory where
  build = genericF

data GovernanceToken = GovernanceToken
  { gtAddress :: Address
  , gtTokenId :: FA2.TokenId
  } deriving stock (Eq, Show)

instance Buildable GovernanceToken where
  build = genericF

customGeneric "GovernanceToken" ligoLayout
deriving anyclass instance IsoValue GovernanceToken

data QuorumThresholdAtCycle = QuorumThresholdAtCycle
  { qaLastUpdatedCycle :: Natural
  , qaQuorumThreshold :: QuorumThreshold
  , qaStaked :: Natural
  } deriving stock (Eq, Show)

customGeneric "QuorumThresholdAtCycle" ligoLayout
deriving anyclass instance IsoValue QuorumThresholdAtCycle

instance Buildable QuorumThresholdAtCycle where
  build = genericF

instance HasAnnotation QuorumThresholdAtCycle where
  annOptions = baseDaoAnnOptions

data Storage' big_map = Storage'
  { sAdmin :: Address
  , sGuardian :: Address
  , sExtra :: ContractExtra' big_map
  , sFrozenTokenId :: FA2.TokenId
  , sMetadata :: TZIP16.MetadataMap big_map
  , sPendingOwner :: Address
  , sPermitsCounter :: Nonce
  , sProposals :: big_map ProposalKey Proposal
  , sProposalKeyListSortByDate :: Set (Natural, ProposalKey)
  , sGovernanceToken :: GovernanceToken
  , sFreezeHistory :: big_map Address AddressFreezeHistory
  , sStartLevel :: Natural
  , sQuorumThresholdAtCycle :: QuorumThresholdAtCycle
  , sFrozenTotalSupply :: Natural
  , sDelegates :: Delegates' big_map
  }

customGeneric "Storage'" ligoLayout

type Storage = Storage' BigMap
deriving stock instance Show Storage
deriving stock instance Eq Storage
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = baseDaoAnnOptions
instance Buildable Storage where
  build = genericF

type StorageView = Storage' BigMapId
deriving stock instance Show StorageView
deriving anyclass instance IsoValue StorageView


instance HasAnnotation GovernanceToken where
  annOptions = baseDaoAnnOptions

instance HasAnnotation Proposal where
  annOptions = baseDaoAnnOptions

instance HasAnnotation FixedFee where
  annOptions = baseDaoAnnOptions
instance Buildable FixedFee where
  build = genericF

instance HasAnnotation AddressFreezeHistory where
  annOptions = baseDaoAnnOptions

instance HasFieldOfType Storage name field => StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

mkStorage
  :: "admin" :! Address
  -> "extra" :! ContractExtra
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> "level" :! Natural
  -> "tokenAddress" :! Address
  -> "quorumThreshold" :! QuorumThreshold
  -> Storage
mkStorage admin extra metadata lvl tokenAddress qt =
  Storage'
    { sAdmin = arg #admin admin
    , sGuardian = arg #admin admin
    , sExtra = arg #extra extra
    , sMetadata = arg #metadata metadata
    , sPendingOwner = arg #admin admin
    , sPermitsCounter = Nonce 0
    , sProposals = mempty
    , sProposalKeyListSortByDate = mempty
    , sGovernanceToken = GovernanceToken
        { gtAddress = arg #tokenAddress tokenAddress
        , gtTokenId = FA2.theTokenId
        }
    , sFreezeHistory = mempty
    , sStartLevel = arg #level lvl
    , sFrozenTokenId = frozenTokenId
    , sQuorumThresholdAtCycle = QuorumThresholdAtCycle 1 (arg #quorumThreshold qt) 0
    , sFrozenTotalSupply = 0
    , sDelegates = mempty
    }

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

newtype FixedFee = FixedFee Natural
  deriving stock (Show, Generic, Eq)
  deriving newtype (Num)
  deriving anyclass IsoValue

data DecisionLambdaInput big_map = DecisionLambdaInput
  { diProposal :: Proposal
  , diExtra :: ContractExtra' big_map
  }

customGeneric "DecisionLambdaInput" ligoLayout

deriving anyclass instance IsoValue (DecisionLambdaInput BigMap)
deriving anyclass instance IsoValue (DecisionLambdaInput BigMapId)
deriving stock instance Show (DecisionLambdaInput BigMap)

instance HasAnnotation (DecisionLambdaInput BigMap) where
  annOptions = baseDaoAnnOptions

data DecisionLambdaOutput big_map = DecisionLambdaOutput
  { doOperations :: List Operation
  , doExtra :: ContractExtra' big_map
  , doGuardian :: Maybe Address
  }

customGeneric "DecisionLambdaOutput" ligoLayout

deriving anyclass instance IsoValue (DecisionLambdaOutput BigMap)
deriving anyclass instance IsoValue (DecisionLambdaOutput BigMapId)
deriving stock instance Show (DecisionLambdaOutput BigMap)

instance HasAnnotation (DecisionLambdaOutput BigMap) where
  annOptions = baseDaoAnnOptions

data Config' big_map = Config'
  { cProposalCheck :: '[ProposeParams, ContractExtra' big_map] :-> '[()]
  , cRejectedProposalSlashValue :: '[Proposal, ContractExtra' big_map]
      :-> '["slash_amount" :! Natural]
  , cDecisionLambda :: '[DecisionLambdaInput big_map] :-> '[DecisionLambdaOutput big_map]

  , cMaxProposals :: Natural
  , cMaxVoters :: Natural
  , cMaxQuorumThreshold :: QuorumFraction
  , cMinQuorumThreshold :: QuorumFraction

  , cFixedProposalFee :: FixedFee
  , cPeriod :: Period
  , cMaxQuorumChange :: QuorumFraction
  , cQuorumChange :: QuorumFraction
  , cGovernanceTotalSupply :: GovernanceTotalSupply
  , cProposalFlushLevel :: Natural
  , cProposalExpiredLevel :: Natural

  , cCustomEntrypoints :: CustomEntrypoints' big_map
  }

customGeneric "Config'" ligoLayout

type Config = Config' BigMap
deriving stock instance Show Config
deriving stock instance Eq Config
deriving anyclass instance IsoValue Config
instance HasAnnotation Config where
  annOptions = baseDaoAnnOptions
instance Buildable Config where
  build = genericF

type ConfigView = Config' BigMapId
deriving stock instance Show ConfigView
deriving anyclass instance IsoValue ConfigView

mkConfig
  :: [CustomEntrypoint]
  -> Period
  -> FixedFee
  -> Natural
  -> Natural
  -> GovernanceTotalSupply
  -> Config
mkConfig customEps votingPeriod fixedProposalFee maxChangePercent changePercent governanceTotalSupply = Config'
  { cProposalCheck = do
      dropN @2; push ()
  , cRejectedProposalSlashValue = do
      dropN @2; push (0 :: Natural); toNamed #slash_amount
  , cDecisionLambda = do
      toField #diExtra
      nil
      swap
      dip (push Nothing)
      constructStack @(DecisionLambdaOutput BigMap)
  , cCustomEntrypoints = DynamicRec' $ BigMap $ M.fromList customEps
  , cFixedProposalFee = fixedProposalFee
  , cPeriod = votingPeriod
  , cProposalFlushLevel = (unPeriod votingPeriod) * 2
  , cProposalExpiredLevel = (unPeriod votingPeriod) * 3
  , cMaxQuorumChange = QuorumFraction $ fromIntegral $ percentageToFractionNumerator maxChangePercent
  , cQuorumChange = QuorumFraction $ fromIntegral $ percentageToFractionNumerator changePercent
  , cGovernanceTotalSupply = governanceTotalSupply
  , cMaxQuorumThreshold = percentageToFractionNumerator 99 -- 99%
  , cMinQuorumThreshold = percentageToFractionNumerator 1 -- 1%

  , cMaxVoters = 1000
  , cMaxProposals = 500
  }

defaultConfig :: Config
defaultConfig = mkConfig [] 10 0 19 5 500

data FullStorage' big_map = FullStorage'
  { fsStorage :: Storage' big_map
  , fsConfig :: Config' big_map
  }

-- Note: FullStorage is a tuple in ligo, so we need to use `ligoCombLayout`
-- Derive Generic on `FullStorage'` does not work.
customGeneric "FullStorage'" ligoCombLayout

type FullStorage = FullStorage' BigMap
deriving stock instance Show FullStorage
deriving stock instance Eq FullStorage
deriving anyclass instance IsoValue FullStorage
instance HasAnnotation FullStorage where
  annOptions = baseDaoAnnOptions

instance Buildable FullStorage where
  build = genericF

-- | Represents a storage value retrieved using the Tezos RPC.
--
-- 'FullStorageView' is very similar to 'FullStorage',
-- except 'BigMap's have been replaced by 'BigMapId'.
-- This is because, when a contract's storage is queried, the Tezos RPC returns
-- big_maps' IDs instead of their contents.
type FullStorageView = FullStorage' BigMapId
deriving stock instance Show FullStorageView
deriving anyclass instance IsoValue FullStorageView

mkFullStorage
  :: "admin" :! Address
  -> "votingPeriod" :? Period
  -> "quorumThreshold" :? QuorumThreshold
  -> "maxChangePercent" :? Natural
  -> "changePercent" :? Natural
  -> "governanceTotalSupply" :? GovernanceTotalSupply
  -> "extra" :! ContractExtra
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> "level" :! Natural
  -> "tokenAddress" :! Address
  -> "customEps" :? [CustomEntrypoint]
  -> FullStorage
mkFullStorage admin vp qt mcp cp gts extra mdt lvl tokenAddress cEps = FullStorage'
  { fsStorage = mkStorage admin extra mdt lvl tokenAddress (#quorumThreshold (argDef #quorumThreshold quorumThresholdDef qt))
  , fsConfig  = mkConfig (argDef #customEps [] cEps)
      (argDef #votingPeriod votingPeriodDef vp) 0 (argDef #maxChangePercent 19 mcp) (argDef #changePercent 5 cp) (argDef #governanceTotalSupply 100 gts)
  }
  where
    quorumThresholdDef = mkQuorumThreshold 1 10 -- 10% of frozen total supply
    votingPeriodDef = 60 * 60 * 24 * 7  -- 7 days

setExtra :: forall a. NicePackedValue a => MText -> a -> FullStorage -> FullStorage
setExtra key v (s@FullStorage' {..}) = s { fsStorage = newStorage }
  where
    (BigMap oldExtra) = unDynamic $ sExtra fsStorage
    newExtra = BigMap $ M.insert key (lPackValueRaw v) oldExtra
    newStorage = fsStorage { sExtra = DynamicRec' newExtra }

-- Instances
------------------------------------------------

deriving anyclass instance IsoValue Voter

customGeneric "Voter" ligoLayout
customGeneric "Proposal" ligoLayout
deriving anyclass instance IsoValue Proposal
instance Buildable Proposal where
  build = genericF


customGeneric "ForbidXTZParam" ligoLayout
deriving anyclass instance IsoValue ForbidXTZParam
instance ParameterHasEntrypoints ForbidXTZParam where
  type ParameterEntrypointsDerivation ForbidXTZParam = EpdDelegate

customGeneric "AllowXTZParam" ligoLayout
deriving anyclass instance IsoValue AllowXTZParam
instance ParameterHasEntrypoints AllowXTZParam where
  type ParameterEntrypointsDerivation AllowXTZParam = EpdDelegate

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate

customGeneric "AddressFreezeHistory" ligoLayout
deriving anyclass instance IsoValue AddressFreezeHistory

customGeneric "VoteParam" ligoLayout
deriving anyclass instance IsoValue VoteParam

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

type instance ErrorArg "nOT_OWNER" = NoErrorArg

instance CustomErrorHasDoc "nOT_OWNER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender of transaction is not owner"

type instance ErrorArg "oPERATION_PROHIBITED" = NoErrorArg

instance CustomErrorHasDoc "oPERATION_PROHIBITED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender tries to update an operator of frozen tokens"

type instance ErrorArg "fROZEN_TOKEN_NOT_TRANSFERABLE" = NoErrorArg

instance CustomErrorHasDoc "fROZEN_TOKEN_NOT_TRANSFERABLE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender tries to transfer frozen token"

type instance ErrorArg "nOT_PENDING_ADMIN" = NoErrorArg

instance CustomErrorHasDoc "nOT_PENDING_ADMIN" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an `accept_ownership` from an address other than what is in the pending owner field"

type instance ErrorArg "nOT_ENOUGH_FROZEN_TOKENS" = ()

instance CustomErrorHasDoc "nOT_ENOUGH_FROZEN_TOKENS" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "There were not enough frozen tokens for the operation"

type instance ErrorArg "nOT_PROPOSING_STAGE" = NoErrorArg

instance CustomErrorHasDoc "nOT_PROPOSING_STAGE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Proposal creation attempted in non-proposal period"

type instance ErrorArg "nOT_ADMIN" = NoErrorArg

instance CustomErrorHasDoc "nOT_ADMIN" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an operation that require administrative privileges\
    \ from an address that is not the current administrator"

type instance ErrorArg "fORBIDDEN_XTZ" = NoErrorArg

instance CustomErrorHasDoc "fORBIDDEN_XTZ" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received some XTZ as part of a contract call, which is forbidden"

type instance ErrorArg "fAIL_PROPOSAL_CHECK" = MText

instance CustomErrorHasDoc "fAIL_PROPOSAL_CHECK" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal that does not pass `proposalCheck`"

type instance ErrorArg "pROPOSAL_NOT_EXIST" = NoErrorArg

instance CustomErrorHasDoc "pROPOSAL_NOT_EXIST" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal that does not exist"

type instance ErrorArg "vOTING_STAGE_OVER" = NoErrorArg

instance CustomErrorHasDoc "vOTING_STAGE_OVER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal that is already ended"

type instance ErrorArg "nOT_DELEGATE" = NoErrorArg

instance CustomErrorHasDoc "nOT_DELEGATE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender of transaction is a delegate of the address"

------------------------------------------------
-- Error causes by bounded value
------------------------------------------------

type instance ErrorArg "mAX_PROPOSALS_REACHED" = NoErrorArg

instance CustomErrorHasDoc "mAX_PROPOSALS_REACHED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal when proposals max amount is already reached"

type instance ErrorArg "mAX_VOTERS_REACHED" = NoErrorArg

instance CustomErrorHasDoc "mAX_VOTERS_REACHED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal when the votes max amount of that proposal is already reached"

type instance ErrorArg "pROPOSER_NOT_EXIST_IN_LEDGER" = NoErrorArg

instance CustomErrorHasDoc "pROPOSER_NOT_EXIST_IN_LEDGER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Expect a proposer address to exist in Ledger but it is not found (Impossible Case)"

type instance ErrorArg "pROPOSAL_NOT_UNIQUE" = NoErrorArg

instance CustomErrorHasDoc "pROPOSAL_NOT_UNIQUE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal that is already existed in the Storage."

type instance ErrorArg "fAIL_TRANSFER_CONTRACT_TOKENS" = NoErrorArg

instance CustomErrorHasDoc "fAIL_TRANSFER_CONTRACT_TOKENS" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to cross-transfer BaseDAO tokens to another contract that does not exist or is not a valid FA2 contract."

type instance ErrorArg "mISSIGNED" = Packed (DataToSign SomeType)

instance CustomErrorHasDoc "mISSIGNED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Invalid signature provided."

type instance ErrorArg "bAD_ENTRYPOINT_PARAMETER" = NoErrorArg

instance CustomErrorHasDoc "bAD_ENTRYPOINT_PARAMETER" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause = "Value passed to the entrypoint is not valid"

type instance ErrorArg "dROP_PROPOSAL_CONDITION_NOT_MET" = NoErrorArg

instance CustomErrorHasDoc "dROP_PROPOSAL_CONDITION_NOT_MET" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Throw when calling `drop_proposal` when the sender is not proposer or guardian and proposal is not expired."

type instance ErrorArg "eXPIRED_PROPOSAL" = NoErrorArg

instance CustomErrorHasDoc "eXPIRED_PROPOSAL" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Throw when trying to `flush` expired proposals."

type instance ErrorArg "eMPTY_FLUSH" = NoErrorArg

instance CustomErrorHasDoc "eMPTY_FLUSH" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Thrown when trying to `flush` with no available proposals."

type instance ErrorArg "nEGATIVE_TOTAL_SUPPLY" = ()

instance CustomErrorHasDoc "nEGATIVE_TOTAL_SUPPLY" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "An error occured when trying to burn an amount of token more than its current total supply"
