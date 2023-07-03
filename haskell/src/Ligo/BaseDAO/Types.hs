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

import Fmt (Buildable, build, GenericBuildable(..))

import Lorentz hiding (div, now)
import Lorentz.Annotation ()
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Lorentz.Contracts.Spec.TZIP16Interface qualified as TZIP16
import Morley.AsRPC (DeriveRPCOptions(..), HasRPCRepr(..), deriveRPCWithOptions, deriveRPC)
import Morley.Michelson.Typed.Annotation
import Morley.Michelson.Typed.Scope
  (ForbidBigMap, ForbidContract, ForbidNestedBigMaps, ForbidOp, ForbidTicket)
import Morley.Michelson.Typed.T (T(TUnit))
import Morley.Michelson.Untyped.Annotation
import Morley.Tezos.Address
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

baseDaoAnnOptions :: Maybe AnnOptions
baseDaoAnnOptions = Just def { fieldAnnModifier = toSnake . dropPrefix }

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
  deriving Buildable via GenericBuildable ProposeParams

instance TypeHasDoc ProposalMetadata => TypeHasDoc ProposeParams where
  typeDocMdDescription =
     "Describes the how many proposer's frozen tokens will be frozen and the proposal metadata"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @ProposeParams
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @ProposeParams

instance HasAnnotation ProposeParams where
  annOptions = baseDaoAnnOptions

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
  deriving Buildable via GenericBuildable QuorumThreshold

fractionDenominator :: Integer
fractionDenominator = 1000000

percentageToFractionNumerator :: Integral p => p -> p
percentageToFractionNumerator p = p * (fromInteger $ div fractionDenominator 100)

mkQuorumThreshold :: Integer -> Integer -> QuorumThreshold
mkQuorumThreshold n d = QuorumThreshold $ fromInteger $ div (n * fractionDenominator) d

instance HasAnnotation QuorumThreshold where
  annOptions = baseDaoAnnOptions

-- | Stages length, in seconds
newtype Period = Period { unPeriod :: Natural}
  deriving stock (Generic, Show, Eq)
  deriving newtype Num
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable Period

instance HasAnnotation Period where
  annOptions = baseDaoAnnOptions

newtype QuorumFraction = QuorumFraction Integer
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable QuorumFraction

instance HasAnnotation QuorumFraction where
  annOptions = baseDaoAnnOptions

-- | Voting period in seconds
newtype GovernanceTotalSupply = GovernanceTotalSupply Natural
  deriving stock (Generic, Show, Eq)
  deriving newtype Num
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable GovernanceTotalSupply

instance HasAnnotation GovernanceTotalSupply where
  annOptions = baseDaoAnnOptions

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
deriving via GenericBuildable VoteParam instance Buildable VoteParam

instance TypeHasDoc ProposalMetadata => TypeHasDoc VoteParam where
  typeDocMdDescription = "Describes target proposal id, vote type and vote amount"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @VoteParam
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @VoteParam

instance HasAnnotation VoteParam where
  annOptions = baseDaoAnnOptions

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
  deriving Buildable via GenericBuildable TransferContractTokensParam

instance TypeHasDoc TransferContractTokensParam where
  typeDocMdDescription =
    "Describes an FA2 contract address and the parameter to call its 'transfer' entrypoint"

instance HasAnnotation TransferContractTokensParam where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Delegates
------------------------------------------------------------------------

data Delegate = Delegate
  { dOwner :: Address
  , dDelegate :: Address
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable Delegate

instance TypeHasDoc Delegate where
  typeDocMdDescription =
    "Describes a relation of a delegate address and an owner address"

instance HasAnnotation Delegate where
  annOptions = baseDaoAnnOptions

type Delegates' (big_map :: Type -> Type -> Type) = big_map Delegate ()

type Delegates = Delegates' BigMap

data DelegateParam = DelegateParam
  { dpEnable :: Bool
  , dpDelegate :: Address
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable DelegateParam

instance TypeHasDoc ProposalMetadata => TypeHasDoc DelegateParam where
  typeDocMdDescription =
     "Describes the parameters to update/remove a delegate."
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @DelegateParam
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @DelegateParam

instance HasAnnotation DelegateParam where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Permissions
------------------------------------------------------------------------

newtype Nonce = Nonce { unNonce :: Natural }
  deriving stock (Generic, Show, Eq)
  deriving newtype (IsoValue, HasAnnotation)
  deriving Buildable via GenericBuildable Nonce

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
    deriving Buildable via GenericBuildable (PermitProtected a)

deriving stock instance Eq a => Eq (PermitProtected a)

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
deriving via GenericBuildable (DynamicRec n) instance Buildable (DynamicRec n)

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
deriving via GenericBuildable ForbidXTZParam instance Buildable ForbidXTZParam

instance ParameterHasEntrypoints ForbidXTZParam where
  type ParameterEntrypointsDerivation ForbidXTZParam = EpdDelegate

data AllowXTZParamContract
  = Propose ProposeParams
  | Transfer_contract_tokens TransferContractTokensParam
  | Transfer_ownership TransferOwnershipParam
  | Accept_ownership ()
  | Default ()
  deriving stock (Eq, Show)

customGeneric "AllowXTZParamContract" ligoLayout
deriving anyclass instance IsoValue AllowXTZParamContract
deriving via GenericBuildable AllowXTZParamContract instance Buildable AllowXTZParamContract
instance ParameterHasEntrypoints AllowXTZParamContract where
  type ParameterEntrypointsDerivation AllowXTZParamContract = EpdDelegate

instance HasAnnotation AllowXTZParamContract where
  annOptions = baseDaoAnnOptions

data AllowXTZParam cep
  = ConcreteEp AllowXTZParamContract
  | CustomEp cep
  deriving stock (Eq, Show)

customGeneric "AllowXTZParam" ligoLayout
deriving anyclass instance IsoValue cep => IsoValue (AllowXTZParam cep)
deriving via GenericBuildable (AllowXTZParam cep)
  instance Buildable cep => Buildable (AllowXTZParam cep)

instance ParameterHasEntrypoints (AllowXTZParam ()) where
  type ParameterEntrypointsDerivation (AllowXTZParam ()) = EpdDelegate

data Parameter' cep
  = XtzAllowed (AllowXTZParam cep)
  | XtzForbidden ForbidXTZParam
  deriving stock (Eq, Show)

type Parameter = Parameter' ()

customGeneric "Parameter'" ligoLayout
deriving anyclass instance IsoValue cep => IsoValue (Parameter' cep)
deriving via GenericBuildable (Parameter' cep) instance Buildable cep => Buildable (Parameter' cep)

instance ParameterHasEntrypoints (Parameter' ()) where
  type ParameterEntrypointsDerivation (Parameter' ()) = EpdDelegate

data AddressFreezeHistory = AddressFreezeHistory
  { fhCurrentStageNum :: Natural
  , fhStaked :: Natural
  , fhCurrentUnstaked :: Natural
  , fhPastUnstaked :: Natural
  } deriving stock (Eq, Show)

customGeneric "AddressFreezeHistory" ligoLayout
deriving anyclass instance IsoValue AddressFreezeHistory
deriving via GenericBuildable AddressFreezeHistory instance Buildable AddressFreezeHistory

data GovernanceToken = GovernanceToken
  { gtAddress :: Address
  , gtTokenId :: FA2.TokenId
  } deriving stock (Eq, Show)

customGeneric "GovernanceToken" ligoLayout
deriving anyclass instance IsoValue GovernanceToken
deriving via GenericBuildable GovernanceToken instance Buildable GovernanceToken

data QuorumThresholdAtCycle = QuorumThresholdAtCycle
  { qaQuorumThreshold :: QuorumThreshold
  , qaLastUpdatedCycle :: Natural
  , qaStaked :: Natural
  } deriving stock (Eq, Show)

customGeneric "QuorumThresholdAtCycle" ligoLayout
deriving anyclass instance IsoValue QuorumThresholdAtCycle
deriving via GenericBuildable QuorumThresholdAtCycle instance Buildable QuorumThresholdAtCycle

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
    deriving anyclass IsoValue
    deriving Buildable via GenericBuildable ProposalDoublyLinkedList

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
  deriving Buildable via GenericBuildable FixedFee

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
  } deriving stock (Show, Eq)

customGeneric "Config" ligoLayout

instance HasAnnotation FixedFee where
  annOptions = baseDaoAnnOptions

deriving anyclass instance IsoValue Config
instance HasAnnotation Config where
  annOptions = baseDaoAnnOptions
deriving via GenericBuildable Config instance Buildable Config

deriveRPCWithOptions "Config" def{droStrategy=ligoLayout}

deriving via GenericBuildable Proposal instance Buildable Proposal

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
  } deriving stock (Show, Eq)

customGeneric "StorageSkeleton" ligoLayout

deriving via GenericBuildable (StorageSkeleton ce)
  instance Buildable ce => Buildable (StorageSkeleton ce)
instance HasAnnotation ce => HasAnnotation (StorageSkeleton ce) where
  annOptions = baseDaoAnnOptions
deriving anyclass instance IsoValue ce => IsoValue (StorageSkeleton ce)

deriveRPCWithOptions "StorageSkeleton" def{droStrategy=ligoLayout, droRecursive=False}
type StorageRPC = StorageSkeletonRPC (VariantToExtra 'Base)

type Storage = StorageSkeleton (VariantToExtra 'Base)

instance HasFieldOfType (StorageSkeleton (ContractExtra' BigMap)) name field => StoreHasField (StorageSkeleton (ContractExtra' BigMap)) name field where
  storeFieldOps = storeFieldOpsADT

mkStorage
  :: "admin" :! ImplicitAddress
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
    { sAdmin = toAddress admin
    , sConfig = config
    , sGuardian = toAddress admin
    , sExtra = extra
    , sMetadata = metadata
    , sPendingOwner = toAddress admin
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
  :: "metadataHostAddress" :! ContractAddress
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
  :: forall cep. "admin" :! ImplicitAddress
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
  , ForbidTicket (ToT (AsRPC (VariantToExtra cep)))
  , ForbidBigMap (ToT (AsRPC (VariantToExtra cep)))
  , ForbidOp (ToT (AsRPC (VariantToExtra cep)))
  , ForbidContract (ToT (AsRPC (VariantToExtra cep)))
  , ForbidNestedBigMaps (ToT (AsRPC (VariantToExtra cep)))
  , ForbidTicket (ToT (VariantToExtra cep))
  , ForbidOp (ToT (VariantToExtra cep))
  , ForbidContract (ToT (VariantToExtra cep))
  , ForbidNestedBigMaps (ToT (VariantToExtra cep))
  , HasAnnotation (VariantToExtra cep)
  , IsoValue (VariantToExtra cep)
  , IsoValue (AsRPC (VariantToExtra cep))
  , ForbidOp (ToT (VariantToExtra cep))
  )
