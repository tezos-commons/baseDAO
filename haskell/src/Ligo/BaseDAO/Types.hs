-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
{-# OPTIONS_GHC -Wno-orphans -Wno-identities #-}

-- | Types mirrored from LIGO implementation.
module Ligo.BaseDAO.Types
  ( VariantToParam
  , VariantToExtra
  , Variants(..)
  , frozenTokenId
  , baseDaoAnnOptions

    -- * Proposals
  , DecisionCallbackInput'(..)
  , DecisionCallbackInput
  , DecisionCallbackOutput
  , ProposalKey
  , ProposeParams(..)
  , GovernanceToken(..)
  , ProposalDoublyLinkedList(..)
  , ProposalDoublyLinkedListRPC(..)

    -- * Voting
  , QuorumThreshold (..)
  , QuorumThresholdAtCycle (..)
  , Period (..)
  , VoteParam (..)
  , QuorumFraction (..)
  , GovernanceTotalSupply (..)
  , StakedVote
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
  , CEConstraints
  , Proposal (..)
  , Parameter
  , Parameter' (..)
  , ForbidXTZParam (..)
  , AllowXTZParamContract (..)
  , AllowXTZParam (..)
  , FreezeParam
  , UnfreezeParam
  , UnstakeVoteParam
  , HasBaseDAOEp

  , FixedFee (..)
  , Storage
  , StorageRPC
  , StorageSkeleton (..)
  , StorageSkeletonRPC (..)
  , Config (..)
  , ConfigRPC (..)
  , ContractExtraConstrain
  , AddressFreezeHistory (..)
  , DynamicRec
  , DynamicRec' (..)
  , dynRecUnsafe
  , mkStorage
  , mkMetadataMap
  , mkConfig
  , defaultConfig
  , mkStorage'
  , setExtra
  , prev
  , next
  ) where

import Universum (Enum, Integral, Num, One(..), Real, Type, Typeable, div, fromIntegral, maybe, (*))

import Fmt (Buildable, build, genericF)

import Lorentz hiding (div, now)
import Lorentz.Annotation ()
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Lorentz.Contracts.Spec.TZIP16Interface qualified as TZIP16
import Morley.AsRPC (HasRPCRepr(..), deriveRPC, deriveRPCWithStrategy)
import Morley.Michelson.Typed.Annotation
import Morley.Michelson.Typed.Scope
  (HasNoBigMap, HasNoContract, HasNoNestedBigMaps, HasNoOp, HasNoTicket)
import Morley.Michelson.Typed.T (T(TUnit))
import Morley.Michelson.Untyped.Annotation
import Morley.Util.Markdown
import Morley.Util.Named
import Test.Cleveland.Instances ()

-- | A data type to track and specify the different DAO variants in existence
-- This became necessary as the custom entrypoints became undynamic (ie no
-- longer decoded from a bytestring), and hence the different variants ended up
-- with slightly different parameters.
data Variants
  = Registry
  | Treasury
  | Lambda
  | LambdaRegistry
  | LambdaTreasury
  | Base

-- | A type family to map a variant to the type that represents it's custom entrypoints.
type family VariantToParam (v :: Variants) :: Type
type instance VariantToParam 'Base = ()

type family VariantToExtra (v :: Variants) :: Type

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

type ProposalMetadata = ByteString

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

percentageToFractionNumerator :: Integral p => p -> p
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

data VoteParam = VoteParam
  { vProposalKey :: ProposalKey
  , vVoteType    :: VoteType
  , vVoteAmount  :: Natural
  , vFrom        :: Address
  }
  deriving stock (Eq, Show)

customGeneric "VoteParam" ligoLayout
deriving anyclass instance IsoValue VoteParam

instance TypeHasDoc ProposalMetadata => TypeHasDoc VoteParam where
  typeDocMdDescription = "Describes target proposal id, vote type and vote amount"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @VoteParam
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @VoteParam

instance HasAnnotation VoteParam where
  annOptions = baseDaoAnnOptions

instance Buildable VoteParam where
  build = genericF


type StakedVote = Natural

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

type ContractExtra' big_map = DynamicRec' big_map "ce"
type ContractExtra = ContractExtra' BigMap

type instance VariantToExtra 'Base = ()

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

  , plQuorumThreshold         :: QuorumThreshold
  }
  deriving stock (Eq, Show)

customGeneric "Proposal" ligoLayout
deriving anyclass instance IsoValue Proposal

-- | Utility type containing an entrypoint name and its packed argument.
type CallCustomParam = (MText, ByteString)

type FreezeParam = ("amount" :! Natural)
type UnfreezeParam = ("amount" :! Natural)
type UnstakeVoteParam = [ProposalKey]

data ForbidXTZParam
  = Drop_proposal ProposalKey
  | Vote [PermitProtected VoteParam]
  | Flush Natural
  | Freeze FreezeParam
  | Unfreeze UnfreezeParam
  | Update_delegate [DelegateParam]
  | Unstake_vote UnstakeVoteParam
  deriving stock (Eq, Show)

customGeneric "ForbidXTZParam" ligoLayout
deriving anyclass instance IsoValue ForbidXTZParam
instance ParameterHasEntrypoints ForbidXTZParam where
  type ParameterEntrypointsDerivation ForbidXTZParam = EpdDelegate

instance Buildable ForbidXTZParam where
  build = genericF

data AllowXTZParamContract
  = Propose ProposeParams
  | Transfer_contract_tokens TransferContractTokensParam
  | Transfer_ownership TransferOwnershipParam
  | Accept_ownership ()
  | Default ()
  deriving stock (Eq, Show)

customGeneric "AllowXTZParamContract" ligoLayout
deriving anyclass instance IsoValue AllowXTZParamContract
instance ParameterHasEntrypoints AllowXTZParamContract where
  type ParameterEntrypointsDerivation AllowXTZParamContract = EpdDelegate

instance HasAnnotation AllowXTZParamContract where
  annOptions = baseDaoAnnOptions

instance Buildable AllowXTZParamContract where
  build = genericF

data AllowXTZParam cep
  = ConcreteEp AllowXTZParamContract
  | CustomEp cep
  deriving stock (Eq, Show)

customGeneric "AllowXTZParam" ligoLayout
deriving anyclass instance IsoValue cep => IsoValue (AllowXTZParam cep)

instance ParameterHasEntrypoints (AllowXTZParam ()) where
  type ParameterEntrypointsDerivation (AllowXTZParam ()) = EpdDelegate

instance Buildable cep => Buildable (AllowXTZParam cep) where
  build = genericF

data Parameter' cep
  = XtzAllowed (AllowXTZParam cep)
  | XtzForbidden ForbidXTZParam
  deriving stock (Eq, Show)

type Parameter = Parameter' ()

customGeneric "Parameter'" ligoLayout
deriving anyclass instance IsoValue cep => IsoValue (Parameter' cep)
instance ParameterHasEntrypoints (Parameter' ()) where
  type ParameterEntrypointsDerivation (Parameter' ()) = EpdDelegate

instance Buildable cep => Buildable (Parameter' cep) where
  build = genericF

data AddressFreezeHistory = AddressFreezeHistory
  { fhCurrentStageNum :: Natural
  , fhStaked :: Natural
  , fhCurrentUnstaked :: Natural
  , fhPastUnstaked :: Natural
  } deriving stock (Eq, Show)

customGeneric "AddressFreezeHistory" ligoLayout
deriving anyclass instance IsoValue AddressFreezeHistory

instance Buildable AddressFreezeHistory where
  build = genericF

data GovernanceToken = GovernanceToken
  { gtAddress :: Address
  , gtTokenId :: FA2.TokenId
  } deriving stock (Eq, Show)

customGeneric "GovernanceToken" ligoLayout
deriving anyclass instance IsoValue GovernanceToken

instance Buildable GovernanceToken where
  build = genericF

data QuorumThresholdAtCycle = QuorumThresholdAtCycle
  { qaQuorumThreshold :: QuorumThreshold
  , qaLastUpdatedCycle :: Natural
  , qaStaked :: Natural
  } deriving stock (Eq, Show)

customGeneric "QuorumThresholdAtCycle" ligoLayout
deriving anyclass instance IsoValue QuorumThresholdAtCycle

instance Buildable QuorumThresholdAtCycle where
  build = genericF

instance HasAnnotation QuorumThresholdAtCycle where
  annOptions = baseDaoAnnOptions


type Direction = Bool

prev, next :: Direction
prev = False
next = True

data ProposalDoublyLinkedList = ProposalDoublyLinkedList
  { plFirst :: ProposalKey
  , plLast :: ProposalKey
  , plMap :: BigMap (ProposalKey, Direction) ProposalKey
  } deriving stock (Generic, Eq, Show)


deriving anyclass instance  IsoValue ProposalDoublyLinkedList
instance Buildable ProposalDoublyLinkedList where
  build = genericF
instance HasAnnotation ProposalDoublyLinkedList where
  annOptions = baseDaoAnnOptions

deriveRPC "ProposalDoublyLinkedList"

instance HasRPCRepr (DynamicRec s) where
  type AsRPC (DynamicRec s) = DynamicRecView s
instance HasRPCRepr FA2.TokenId where
  type AsRPC FA2.TokenId = FA2.TokenId
instance HasRPCRepr GovernanceToken where
  type AsRPC GovernanceToken = GovernanceToken
instance HasRPCRepr Nonce where
  type AsRPC Nonce = Nonce
instance HasRPCRepr QuorumThresholdAtCycle where
  type AsRPC QuorumThresholdAtCycle = QuorumThresholdAtCycle
instance HasRPCRepr GovernanceTotalSupply where
  type AsRPC GovernanceTotalSupply = GovernanceTotalSupply
instance HasRPCRepr QuorumFraction where
  type AsRPC QuorumFraction = QuorumFraction
instance HasRPCRepr Period where
  type AsRPC Period = Period

newtype FixedFee = FixedFee Natural
  deriving stock (Show, Generic, Eq)
  deriving newtype (Num)
  deriving anyclass IsoValue

instance Buildable FixedFee where
  build = genericF

instance HasRPCRepr FixedFee where
  type AsRPC FixedFee = FixedFee

data Config = Config
  { cMaxQuorumThreshold :: QuorumFraction
  , cMinQuorumThreshold :: QuorumFraction

  , cFixedProposalFee :: FixedFee
  , cPeriod :: Period
  , cMaxQuorumChange :: QuorumFraction
  , cQuorumChange :: QuorumFraction
  , cGovernanceTotalSupply :: GovernanceTotalSupply
  , cProposalFlushLevel :: Natural
  , cProposalExpiredLevel :: Natural
  }

customGeneric "Config" ligoLayout

instance HasAnnotation FixedFee where
  annOptions = baseDaoAnnOptions

deriving stock instance Show Config
deriving stock instance Eq Config
deriving anyclass instance IsoValue Config
instance HasAnnotation Config where
  annOptions = baseDaoAnnOptions
instance Buildable Config where
  build = genericF

deriveRPCWithStrategy "Config" ligoLayout

instance Buildable Proposal where
  build = genericF

instance HasAnnotation Proposal where
  annOptions = baseDaoAnnOptions

instance HasAnnotation GovernanceToken where
  annOptions = baseDaoAnnOptions

instance HasAnnotation AddressFreezeHistory where
  annOptions = baseDaoAnnOptions

data StorageSkeleton ce = Storage
  { sAdmin :: Address
  , sConfig :: Config
  , sDelegates :: Delegates' BigMap
  , sExtra :: ce
  , sFreezeHistory :: BigMap Address AddressFreezeHistory
  , sFrozenTokenId :: FA2.TokenId
  , sFrozenTotalSupply :: Natural
  , sGovernanceToken :: GovernanceToken
  , sGuardian :: Address
  , sMetadata :: TZIP16.MetadataMap
  , sOngoingProposalsDlist :: Maybe ProposalDoublyLinkedList
  , sPendingOwner :: Address
  , sPermitsCounter :: Nonce
  , sProposals :: BigMap ProposalKey Proposal
  , sQuorumThresholdAtCycle :: QuorumThresholdAtCycle
  , sStakedVotes :: BigMap (Address, ProposalKey) StakedVote
  , sStartLevel :: Natural
  } deriving stock Eq

customGeneric "StorageSkeleton" ligoLayout

deriving stock instance Show ce => Show (StorageSkeleton ce)
instance Buildable ce => Buildable (StorageSkeleton ce) where
  build = genericF
instance HasAnnotation ce => HasAnnotation (StorageSkeleton ce) where
  annOptions = baseDaoAnnOptions
deriving anyclass instance IsoValue ce => IsoValue (StorageSkeleton ce)

deriveRPCWithStrategy "StorageSkeleton" ligoLayout
type StorageRPC = StorageSkeletonRPC (VariantToExtra 'Base)

type Storage = StorageSkeleton (VariantToExtra 'Base)

instance HasFieldOfType (StorageSkeleton (ContractExtra' BigMap)) name field => StoreHasField (StorageSkeleton (ContractExtra' BigMap)) name field where
  storeFieldOps = storeFieldOpsADT

mkStorage
  :: "admin" :! Address
  -> "extra" :! ce
  -> "metadata" :! TZIP16.MetadataMap
  -> "level" :! Natural
  -> "tokenAddress" :! Address
  -> "quorumThreshold" :! QuorumThreshold
  -> "config" :! Config
  -> StorageSkeleton ce
mkStorage
  (arg #admin -> admin)
  (arg #extra -> extra)
  (arg #metadata -> metadata)
  (arg #level -> lvl)
  (arg #tokenAddress -> tokenAddress)
  (arg #quorumThreshold -> qt)
  (arg #config -> config) =
  Storage
    { sAdmin = admin
    , sConfig = config
    , sGuardian = admin
    , sExtra = extra
    , sMetadata = metadata
    , sPendingOwner = admin
    , sPermitsCounter = Nonce 0
    , sProposals = mempty
    , sOngoingProposalsDlist = Nothing
    , sStakedVotes = mempty
    , sGovernanceToken = GovernanceToken
        { gtAddress = tokenAddress
        , gtTokenId = FA2.theTokenId
        }
    , sFreezeHistory = mempty
    , sStartLevel = lvl
    , sFrozenTokenId = frozenTokenId
    , sQuorumThresholdAtCycle = QuorumThresholdAtCycle qt 1 0
    , sFrozenTotalSupply = 0
    , sDelegates = mempty
    }

mkMetadataMap
  :: "metadataHostAddress" :! Address
  -> "metadataHostChain" :? TZIP16.ExtChainId
  -> "metadataKey" :! MText
  -> TZIP16.MetadataMap
mkMetadataMap
  (arg #metadataHostAddress -> hostAddress)
  (argF #metadataHostChain -> hostChain)
  (arg #metadataKey -> key) =
  TZIP16.metadataURI . TZIP16.tezosStorageUri host $ key
  where
    host = maybe
      TZIP16.contractHost
      TZIP16.foreignContractHost
      hostChain
      hostAddress

data DecisionCallbackInput' ce = DecisionCallbackInput'
  { diProposal :: Proposal
  , diExtra :: ce
  }

customGeneric "DecisionCallbackInput'" ligoLayout

deriving anyclass instance IsoValue ce => IsoValue (DecisionCallbackInput' ce)
deriving stock instance Show ce => Show (DecisionCallbackInput' ce)

type DecisionCallbackInput = DecisionCallbackInput' ()

instance HasAnnotation ce => HasAnnotation (DecisionCallbackInput' ce) where
  annOptions = baseDaoAnnOptions

data DecisionCallbackOutput' ce = DecisionCallbackOutput'
  { doOperations :: List Operation
  , doExtra :: ce
  , doGuardian :: Maybe Address
  }

type DecisionCallbackOutput = DecisionCallbackOutput' ()

customGeneric "DecisionCallbackOutput'" ligoLayout

deriving anyclass instance IsoValue ce => IsoValue (DecisionCallbackOutput' ce)
deriving stock instance Show ce => Show (DecisionCallbackOutput' ce)

instance HasAnnotation ce => HasAnnotation (DecisionCallbackOutput' ce) where
  annOptions = baseDaoAnnOptions

mkConfig
  :: Period
  -> FixedFee
  -> Natural
  -> Natural
  -> GovernanceTotalSupply
  -> Config
mkConfig votingPeriod fixedProposalFee maxChangePercent changePercent governanceTotalSupply = Config
  { cFixedProposalFee = fixedProposalFee
  , cPeriod = votingPeriod
  , cProposalFlushLevel = (unPeriod votingPeriod) * 2
  , cProposalExpiredLevel = (unPeriod votingPeriod) * 3
  , cMaxQuorumChange = QuorumFraction $ fromIntegral $ percentageToFractionNumerator maxChangePercent
  , cQuorumChange = QuorumFraction $ fromIntegral $ percentageToFractionNumerator changePercent
  , cGovernanceTotalSupply = governanceTotalSupply
  , cMaxQuorumThreshold = percentageToFractionNumerator 99 -- 99%
  , cMinQuorumThreshold = percentageToFractionNumerator 1 -- 1%
  }

defaultConfig :: Config
defaultConfig = mkConfig (Period 20) (FixedFee 0) 19 5 (GovernanceTotalSupply 500)

type ContractExtraConstrain ce = (NiceStorage ce, NiceUnpackedValue (AsRPC ce))

setExtra :: (ce -> ce) -> StorageSkeleton ce -> StorageSkeleton ce
setExtra fn fsk = fsk { sExtra = fn $ sExtra fsk }

mkStorage'
  :: forall cep. "admin" :! Address
  -> "votingPeriod" :? Period
  -> "quorumThreshold" :? QuorumThreshold
  -> "maxChangePercent" :? Natural
  -> "changePercent" :? Natural
  -> "governanceTotalSupply" :? GovernanceTotalSupply
  -> "extra" :! (VariantToExtra cep)
  -> "metadata" :! TZIP16.MetadataMap
  -> "level" :! Natural
  -> "tokenAddress" :! Address
  -> StorageSkeleton (VariantToExtra cep)
mkStorage' admin vp qt mcp cp gts extra mdt lvl tokenAddress = let
  config = mkConfig
      (argDef #votingPeriod votingPeriodDef vp) (FixedFee 0) (argDef #maxChangePercent 19 mcp) (argDef #changePercent 5 cp) (argDef #governanceTotalSupply (GovernanceTotalSupply 100) gts)
  in mkStorage admin extra mdt lvl tokenAddress (#quorumThreshold (argDef #quorumThreshold quorumThresholdDef qt)) (#config config)
  where
    quorumThresholdDef = mkQuorumThreshold 1 10 -- 10% of frozen total supply
    votingPeriodDef = Period $ 60 * 60 * 24 * 7  -- 7 days


type HasBaseDAOEp a = (HasDefEntrypointArg a (EntrypointRef 'Nothing) (), ParameterContainsEntrypoints a
  '[ "Propose" :> ProposeParams
   , "Transfer_contract_tokens" :> TransferContractTokensParam
   , "Transfer_ownership" :> TransferOwnershipParam
   , "Accept_ownership" :> ()
   , "Vote" :> [PermitProtected VoteParam]
   , "Flush" :> Natural
   , "Unfreeze" :> UnfreezeParam
   , "Freeze" :> FreezeParam
   , "Drop_proposal" :> ProposalKey
   , "Update_delegate" :> [DelegateParam]
   , "Unstake_vote" :> UnstakeVoteParam
   ])

type CEConstraints cep =
  ( Typeable cep
  , Typeable (VariantToExtra cep)
  , Typeable (AsRPC (VariantToExtra cep))
  , HasNoTicket (ToT (AsRPC (VariantToExtra cep)))
  , HasNoBigMap (ToT (AsRPC (VariantToExtra cep)))
  , HasNoOp (ToT (AsRPC (VariantToExtra cep)))
  , HasNoContract (ToT (AsRPC (VariantToExtra cep)))
  , HasNoNestedBigMaps (ToT (AsRPC (VariantToExtra cep)))
  , HasNoTicket (ToT (VariantToExtra cep))
  , HasNoOp (ToT (VariantToExtra cep))
  , HasNoContract (ToT (VariantToExtra cep))
  , HasNoNestedBigMaps (ToT (VariantToExtra cep))
  , HasAnnotation (VariantToExtra cep)
  , IsoValue (VariantToExtra cep)
  , IsoValue (AsRPC (VariantToExtra cep))
  , HasNoOp (ToT (VariantToExtra cep))
  )

