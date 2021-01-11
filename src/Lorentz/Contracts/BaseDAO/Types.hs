-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.BaseDAO.Types
  ( Config(..)
  , defaultConfig

  , Operators
  , Ledger
  , LedgerValue

  , Parameter (..)
  , ProposeParams(..)
  , ProposalKey
  , Proposal (..)
  , VotingPeriod
  , VoteParam (..)
  , VoteType
  , QuorumThreshold
  , BurnParam (..)
  , MintParam (..)
  , TransferContractTokensParam (..)
  , TokenAddressParam

  , Storage (..)
  , StorageC
  , TransferOwnershipParam
  , MigrateParam
  , MigrationStatus

  , mkStorage

  , CachedFunc (..)
  , mkCachedFunc
  , pushCachedFunc
  , callCachedFunc

  , DebitFromFunc
  , CreditToFunc
  , HasFuncContext

  , Entrypoint'

  , Nonce (..)
  , DataToSign (..)
  , Permit (..)
  , pSender
  , PermitProtected (..)
  , pattern NoPermit

  , unfrozenTokenId
  , frozenTokenId

  , Counter
  , baseDaoAnnOptions
  ) where

import Universum (Each, Num (..), Show)

import qualified Data.Kind as Kind

import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Lorentz.Zip
import Michelson.Runtime.GState (genesisAddress)
import Michelson.Typed.Annotation
import Michelson.Typed.T
import Michelson.Untyped.Annotation
import Tezos.Address
import Util.Markdown
import Util.Named
import Util.Type
import Util.TypeLits

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

data Config contractExtra proposalMetadata otherParam = Config
  { cDaoName :: Text
  , cDaoDescription :: Markdown
  , cUnfrozenTokenMetadata :: FA2.TokenMetadata
  , cFrozenTokenMetadata :: FA2.TokenMetadata

  , cProposalCheck :: forall s store.
        StorageC store contractExtra proposalMetadata
    =>  ProposeParams proposalMetadata : store : s
    :-> Bool : s
  -- ^ A lambda used to verify whether a proposal can be submitted.
  -- It checks 2 things: the proposal itself and the amount of tokens frozen upon submission.
  -- It allows the DAO to reject a proposal by arbitrary logic and captures bond requirements

  , cRejectedProposalReturnValue :: forall s store.
        StorageC store contractExtra proposalMetadata
    =>  Proposal proposalMetadata : store : s
    :-> ("slash_amount" :! Natural) : s
  -- ^ When a proposal is rejected, the value that voters get back can be slashed.
  -- This lambda specifies how many tokens will be received.
  -- For example, if Alice freezes 100 tokens to vote and this lambda divides the value by 2,
  -- only 50 tokens will be unfrozen and other 50 tokens will be burnt.

  , cDecisionLambda :: forall s store.
     (StorageC store contractExtra proposalMetadata, HasFuncContext s store)
    => Proposal proposalMetadata : store : s
    :-> List Operation : store : s
  -- ^ The decision lambda is executed based on a successful proposal.

  , cCustomCall :: forall s store.
     (StorageC store contractExtra proposalMetadata, HasFuncContext s store)
    => otherParam : store : s
    :-> List Operation : store : s
  -- ^ Implementation of DAO's custom entrypoints.

  -- Bounded Values
  , cMaxProposals :: Natural
  , cMaxVotes :: Natural -- including both upvotes and downvotes
  , cMaxQuorumThreshold :: Natural
    -- ^ Should not be bigger than 'cMaxVotes' or else if the admin set the
    -- quorum_threshold to max, the threshold could never be met.
  , cMinQuorumThreshold :: Natural
  , cMaxVotingPeriod :: Natural -- Maximum of seconds allow to be set
  , cMinVotingPeriod :: Natural
  }

-- | The default configuration with the simplest possible logic.
defaultConfig :: Config ce pm op
defaultConfig = Config
  { cDaoName = "BaseDAO"
  , cDaoDescription = [md|An example of a very simple DAO contract without any custom checks,
                          extra data and decision lambda.|]
  , cUnfrozenTokenMetadata = FA2.TokenMetadata
      { FA2.tmTokenId = unfrozenTokenId
      , FA2.tmSymbol = [mt|unfrozen_token|]
      , FA2.tmName = [mt|Unfrozen Token|]
      , FA2.tmDecimals = 8
      , FA2.tmExtras = mempty
      }
  , cFrozenTokenMetadata = FA2.TokenMetadata
      { FA2.tmTokenId = frozenTokenId
      , FA2.tmSymbol = [mt|frozen_token|]
      , FA2.tmName = [mt|Frozen Token|]
      , FA2.tmDecimals = 8
      , FA2.tmExtras = mempty
      }
  , cProposalCheck = do
      dropN @2; push True
  , cRejectedProposalReturnValue = do
      dropN @2; push (0 :: Natural); toNamed #slash_amount
  , cDecisionLambda = do
      drop; nil
  , cCustomCall =
      failUsing [mt|CustomCalls are not implemented|]

  , cMaxVotingPeriod = 60 * 60 * 24 * 30
  , cMinVotingPeriod = 1 -- value between 1 second - 1 month

  , cMaxQuorumThreshold = 1000
  , cMinQuorumThreshold = 1

  , cMaxVotes = 1000
  , cMaxProposals = 500
  }

------------------------------------------------------------------------
-- Operators
------------------------------------------------------------------------

type Operator = ("owner" :! Address, "operator" :! Address)

type Operators = BigMap Operator ()

------------------------------------------------------------------------
-- Ledger
------------------------------------------------------------------------

type Ledger = BigMap LedgerKey LedgerValue

type LedgerKey = (Address, FA2.TokenId)

type LedgerValue = Natural

------------------------------------------------------------------------
-- Storage/Parameter
------------------------------------------------------------------------

-- | Migration status of the contract
data MigrationStatus
  = NotInMigration
  | MigratingTo Address
  | MigratedTo Address
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc MigrationStatus where
  typeDocMdDescription =
    "Migration status of the contract"

{- | Storage of a DAO contract.

It has two type parameters that determine DAO-custom pieces that will be stored:

* @contractExtra@ type parameter stands for a contract-global state.
  It can be used, for instance, to collect data about accepted proposals.
  In case you don't have use case for this, initialize it to @'()'@.

* @proposalMetadata@ type parameter defines the data that participants of the
  contract have to include into their proposals.
  This part is initialized when the proposal is created and offered and cannot
  be changed afterwards.

-}
data Storage (contractExtra :: Kind.Type) (proposalMetadata :: Kind.Type) = Storage
  { sLedger       :: Ledger
  , sOperators    :: Operators
  , sTokenAddress :: Address
  , sAdmin        :: Address
  , sPendingOwner :: Address
  , sMigrationStatus :: MigrationStatus

  , sVotingPeriod              :: VotingPeriod
  , sQuorumThreshold           :: QuorumThreshold

  , sExtra                     :: contractExtra
  , sProposals                 :: BigMap (ProposalKey proposalMetadata) (Proposal proposalMetadata)
  , sProposalKeyListSortByDate :: Set (Timestamp, ProposalKey proposalMetadata) -- Oldest first

  , sPermitsCounter :: Nonce

  , sMetadata :: "metadata" :! TZIP16.MetadataMap BigMap
  }
  deriving stock (Generic, Show)

deriving anyclass instance
  (WellTypedIsoValue ce, WellTypedIsoValue pm) =>
  IsoValue (Storage ce  pm)

instance (HasAnnotation ce, HasAnnotation pm) => HasAnnotation (Storage ce pm) where
  annOptions = baseDaoAnnOptions

instance Each [IsoValue, TypeHasDoc] [ce, pm] => TypeHasDoc (Storage ce pm) where
   typeDocMdDescription =
     "Storage type for baseDAO contract"
   typeDocMdReference = poly2TypeDocMdReference
   typeDocHaskellRep = concreteTypeDocHaskellRep @(Storage Natural MText)
   typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Storage Natural MText)

instance HasFieldOfType (Storage ce pm) name field =>
         StoreHasField (Storage ce pm) name field where
  storeFieldOps = storeFieldOpsADT

instance (IsoValue ce, IsoValue pm) =>
    StoreHasSubmap (Storage ce pm) "sLedger" LedgerKey LedgerValue where
  storeSubmapOps = storeSubmapOpsDeeper #sLedger

instance (IsoValue ce, IsoValue pm) =>
    StoreHasSubmap (Storage ce pm) "sOperators" ("owner" :! Address, "operator" :! Address) () where
  storeSubmapOps = storeSubmapOpsDeeper #sOperators

instance (IsoValue ce, KnownValue pm) =>
    StoreHasSubmap (Storage ce pm) "sProposals" (ProposalKey pm) (Proposal pm) where
  storeSubmapOps = storeSubmapOpsDeeper #sProposals

type StorageC store ce pm =
  ( StorageContains store
    [ "sLedger" := LedgerKey ~> LedgerValue
    , "sOperators" := ("owner" :! Address, "operator" :! Address) ~> ()
    , "sTokenAddress" := Address
    , "sAdmin" := Address
    , "sPendingOwner" := Address
    , "sLedger" := Ledger
    , "sMigrationStatus" := MigrationStatus

    , "sVotingPeriod" := VotingPeriod
    , "sQuorumThreshold" := QuorumThreshold

    -- , "sExtra" := Identity ce
    , "sProposals" := ProposalKey pm ~> Proposal pm
    , "sProposalKeyListSortByDate" := Set (Timestamp, ProposalKey pm)

    , "sPermitsCounter" := Nonce
    ]
  , StoreHasField store "sExtra" ce
    -- TODO: remove ^^^ once we use
    -- https://gitlab.com/morley-framework/morley/-/merge_requests/665
  , KnownValue pm
  )

-- | Parameter of the BaseDAO contract
data Parameter proposalMetadata otherParam
  = Accept_ownership ()
  | Burn BurnParam
  | Call_FA2 FA2.Parameter
  | CallCustom otherParam
  | Confirm_migration ()
  | Drop_proposal (ProposalKey proposalMetadata)
  | Flush Natural
  | GetVotePermitCounter (View () Nonce)
  | Migrate MigrateParam
  | Mint MintParam
  | Propose (ProposeParams proposalMetadata)
  | Set_quorum_threshold QuorumThreshold
  | Set_voting_period VotingPeriod
  | Transfer_contract_tokens TransferContractTokensParam
  | Transfer_ownership TransferOwnershipParam
  | Vote [PermitProtected $ VoteParam proposalMetadata]
  deriving stock (Generic, Show)

type TransferOwnershipParam = ("newOwner" :! Address)
type MigrateParam = ("newAddress" :! Address)

-- | Voting period in seconds
type VotingPeriod = Natural

-- | QuorumThreshold that a proposal need to meet
-- A proposal will be rejected if the quorum_threshold is not met,
-- regardless of upvotes > downvotes
-- A proposal will be accepted only if the
-- (quorum_threshold >= upvote + downvote) && (upvote > downvote)
type QuorumThreshold = Natural

mkStorage
  :: "admin" :! Address
  -> "votingPeriod" :? Natural
  -> "quorumThreshold" :? Natural
  -> "extra" :! ce
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> Storage ce pm
mkStorage admin votingPeriod quorumThreshold extra metadata =
  Storage
  { sLedger = mempty
  , sOperators = mempty
  , sTokenAddress = genesisAddress
  , sPendingOwner = arg #admin admin
  , sAdmin = arg #admin admin
  , sMigrationStatus = NotInMigration
  , sVotingPeriod = argDef #votingPeriod votingPeriodDef votingPeriod
  , sQuorumThreshold = argDef #quorumThreshold quorumThresholdDef quorumThreshold

  , sExtra = arg #extra extra
  , sProposals = mempty
  , sProposalKeyListSortByDate = mempty
  , sPermitsCounter = Nonce 0

  , sMetadata = metadata
  }
  where
    votingPeriodDef = 60 * 60 * 24 * 7  -- 7 days
    quorumThresholdDef = 4
    -- ^ any proposals that have less that 4 votes (by default) will be rejected
    -- regardless of upvotes

------------------------------------------------------------------------
-- Optimizations
------------------------------------------------------------------------

-- | Wrapper over code designating that it should not be used directly,
-- rather picked stack where it has been put previously.
-- This makes sense, since typechecking code is in all senses much more
-- expensive than executing it.
--
-- We also attach a name to the function, so that we don't not need
-- to remember the names.
data CachedFunc n i o =
  (Each [KnownList, ZipInstr] [i, o]) =>
  CachedFunc (Label n) (i :-> o)

-- | Construct a 'CachedFunc' object.
mkCachedFunc
  :: (KnownSymbol n, Each [KnownList, ZipInstr] [i, o])
  => i :-> o -> CachedFunc n i o
mkCachedFunc = CachedFunc fromLabel

-- | Push 'CachedFunc' on stack so that it is later accessible by
-- 'callCachedFunc'.
pushCachedFunc
  :: (NiceConstant (i :-> o))
  => CachedFunc n i o
  -> s :-> n :! (i :-> o) : s
pushCachedFunc (CachedFunc Label f) = push (fromLabel .! f)

-- | Call a function that previously was pushed on stack.
--
-- This is semantically equivalent to injecting the code of the
-- given cached function, but requires much less instructions.
--
-- You have to provide the original 'CachedFunc' object here,
-- this is necessary to inherit the documentation of the function.
callCachedFunc
  :: forall i o n s.
     (HasNamedVar (i ++ s) n (i :-> o))
  => CachedFunc n i o
  -> (i ++ s) :-> (o ++ s)
callCachedFunc (CachedFunc l f :: CachedFunc n i o) = do
  cutLorentzNonDoc f
  dupL l
  execute @i @o @s

-- Cached functions context of the contract
------------------------------------------------------------------------

type DebitFromFunc store =
  CachedFunc "debitFrom" [store, Address, (FA2.TokenId, Natural)] '[store]

type CreditToFunc store =
  CachedFunc "creditTo" [store, Address, (FA2.TokenId, Natural)] '[store]

type family IncludesFunc f where
  IncludesFunc (CachedFunc n i o) = n := (i :-> o)

-- | Indicates that we keep some functions on stack in order to include
-- them only once in the contract.
type HasFuncContext s store =
  ( IsoValue store
  , VarIsUnnamed store
  , HasNamedVars s
    [ IncludesFunc (DebitFromFunc store)
    , IncludesFunc (CreditToFunc store)
    ]
  )

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

type Entrypoint' cp st s = (cp : st : s) :-> (([Operation], st) : s)

------------------------------------------------------------------------
-- Permissions
------------------------------------------------------------------------

newtype Nonce = Nonce { unNonce :: Natural }
  deriving stock (Generic, Show)
  deriving newtype (IsoValue, HasAnnotation)

instance TypeHasDoc Nonce where
  typeDocMdDescription =
    "Contract-local nonce used to make some data unique."

-- | When singing something, you usually want to use this wrapper over your data.
-- It ensures that replay attack is not possible.
data DataToSign d = DataToSign
  { dsChainId :: ChainId
  , dsContract :: Address
  , dsNonce :: Nonce
  , dsData :: d
  } deriving stock (Generic)
    deriving anyclass (IsoValue)

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

instance HasAnnotation (Permit a) where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc a => TypeHasDoc (Permit a) where
  typeDocMdDescription = [md|
    Permission for executing an action from another user's behalf.

    This contains public key of that user and signed argument for the entrypoint.
    Type parameter of `Permit` stands for the entrypoint argument type.
    |]
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(Permit Integer)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Permit Integer)

-- | Getter for address of permission's author.
pSender :: Permit a -> Address
pSender = mkKeyAddress . pKey

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

-- | Perform operation from sender behalf.
pattern NoPermit :: a -> PermitProtected a
pattern NoPermit a = PermitProtected a Nothing

instance HasAnnotation a => HasAnnotation (PermitProtected a) where
  getAnnotation _ =
    NTPair
      (ann @TypeTag "permit_protected")
      (noAnn @FieldTag)
      (ann @FieldTag "permit")
      (getAnnotation @a NotFollowEntrypoint)
      (getAnnotation @(Maybe (Permit a)) NotFollowEntrypoint)
    -- TODO: propably it is not assumed to look this way,
    --       rewrite in a prettier way somehow?

instance TypeHasDoc a => TypeHasDoc (PermitProtected a) where
  typeDocMdDescription = [md|
    Marks an entrypoint with given argument type as callable from another
    user's behalf.

    * If `permit` part is present, we use the supplied information to identify
    the original author of the request and validate that it is constructed by
    them.
    * If `permit` part is absent, we assume that entrypoint is called from the
    current sender's behalf.
    |]
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(PermitProtected Integer)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(PermitProtected Integer)

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
-- Proposal
------------------------------------------------------------------------

type ProposalKey pm = Hash Blake2b $ Packed (ProposeParams pm, Address)

-- | Proposal type which will be stored in 'Storage' `sProposals`
-- `pVoters` is needed due to we need to keep track of voters to be able to
-- unfreeze their tokens.
data Proposal proposalMetadata = Proposal
  { pUpvotes             :: Natural
  , pDownvotes           :: Natural
  , pStartDate           :: Timestamp

  , pMetadata            :: proposalMetadata

  , pProposer            :: Address
  , pProposerFrozenToken :: Natural

  , pVoters              :: [(Address, Natural)]
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance (TypeHasDoc pm, IsoValue pm) => TypeHasDoc (Proposal pm) where
  typeDocMdDescription =
    "Contract's storage holding a big_map with all balances and the operators."
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(Proposal ())
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Proposal ())

instance HasAnnotation pm => HasAnnotation (Proposal pm) where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Propose
------------------------------------------------------------------------

data ProposeParams proposalMetadata = ProposeParams
  { ppFrozenToken      :: Natural
  --  ^ Determines how many sender's tokens will be frozen to get
  -- the proposal accepted
  , ppProposalMetadata :: proposalMetadata
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance (TypeHasDoc pm, IsoValue pm) => TypeHasDoc (ProposeParams pm) where
  typeDocMdDescription =
     "Describes the how many proposer's frozen tokens will be frozen and the proposal metadata"
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(ProposeParams ())
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(ProposeParams ())

instance HasAnnotation pm => HasAnnotation (ProposeParams pm) where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Vote
------------------------------------------------------------------------

type VoteType = Bool

data VoteParam pm = VoteParam
  { vProposalKey :: ProposalKey pm
  , vVoteType    :: VoteType
  , vVoteAmount  :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance (TypeHasDoc pm, IsoValue pm) => TypeHasDoc (VoteParam pm) where
  typeDocMdDescription = "Describes target proposal id, vote type and vote amount"
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(VoteParam MText)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(VoteParam MText)

instance HasAnnotation (VoteParam pm) where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Non FA2
------------------------------------------------------------------------

data BurnParam = BurnParam
  { bFrom_   :: Address
  , bTokenId :: FA2.TokenId
  , bAmount  :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc BurnParam where
  typeDocMdDescription = "Describes whose account, which token id and in what amount to burn"

instance HasAnnotation BurnParam where
  annOptions = baseDaoAnnOptions

data MintParam = MintParam
  { mTo_     :: Address
  , mTokenId :: FA2.TokenId
  , mAmount  :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc MintParam where
  typeDocMdDescription = "Describes whose account, which token id and in what amount to mint"

instance HasAnnotation MintParam where
  annOptions = baseDaoAnnOptions

data TransferContractTokensParam = TransferContractTokensParam
  { tcContractAddress :: Address
  , tcParams          :: FA2.TransferParams
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc TransferContractTokensParam where
  typeDocMdDescription = "TODO"

instance HasAnnotation TransferContractTokensParam where
  annOptions = baseDaoAnnOptions

type TokenAddressParam = ContractRef Address

------------------------------------------------------------------------
-- Tokens
------------------------------------------------------------------------

unfrozenTokenId :: FA2.TokenId
unfrozenTokenId = FA2.TokenId 0

frozenTokenId :: FA2.TokenId
frozenTokenId = FA2.TokenId 1

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

-- Useful type when `iter` with a counter
type Counter = (("currentCount" :! Natural), Natural)

baseDaoAnnOptions :: AnnOptions
baseDaoAnnOptions = defaultAnnOptions { fieldAnnModifier = dropPrefixThen toSnake }

------------------------------------------------------------------------
-- Error
------------------------------------------------------------------------

type instance ErrorArg "nOT_OWNER" = ()

instance CustomErrorHasDoc "nOT_OWNER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender of transaction is not owner"

type instance ErrorArg "fROZEN_TOKEN_NOT_TRANSFERABLE" = ()

instance CustomErrorHasDoc "fROZEN_TOKEN_NOT_TRANSFERABLE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender tries to transfer frozen token"

type instance ErrorArg "nOT_PENDING_ADMIN" = ()

instance CustomErrorHasDoc "nOT_PENDING_ADMIN" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an `accept_ownership` from an address other than what is in the pending owner field"

type instance ErrorArg "nOT_ADMIN" = ()

instance CustomErrorHasDoc "nOT_ADMIN" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an operation that require administrative privileges\
    \ from an address that is not the current administrator"

type instance ErrorArg "mIGRATED" = Address

instance CustomErrorHasDoc "mIGRATED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Recieved a call on a migrated contract"

type instance ErrorArg "nOT_MIGRATING" = ()

instance CustomErrorHasDoc "nOT_MIGRATING" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Recieved a confirm_migration call on a contract that is not in migration"

type instance ErrorArg "nOT_MIGRATION_TARGET" = ()

instance CustomErrorHasDoc "nOT_MIGRATION_TARGET" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Recieved a confirm_migration call on a contract from an address other than the new version"

type instance ErrorArg "fORBIDDEN_XTZ" = ()

instance CustomErrorHasDoc "fORBIDDEN_XTZ" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received some XTZ as part of a contract call, which is forbidden"

type instance ErrorArg "fAIL_PROPOSAL_CHECK" = ()

instance CustomErrorHasDoc "fAIL_PROPOSAL_CHECK" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal that does not pass `proposalCheck`"

type instance ErrorArg "pROPOSAL_INSUFFICIENT_BALANCE" = ()

instance CustomErrorHasDoc "pROPOSAL_INSUFFICIENT_BALANCE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal without having enough unfrozen token"

type instance ErrorArg "vOTING_INSUFFICIENT_BALANCE" = ()

instance CustomErrorHasDoc "vOTING_INSUFFICIENT_BALANCE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal without having enough unfrozen token"

type instance ErrorArg "pROPOSAL_NOT_EXIST" = ()

instance CustomErrorHasDoc "pROPOSAL_NOT_EXIST" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal that does not exist"

type instance ErrorArg "vOTING_PERIOD_OVER" = ()

instance CustomErrorHasDoc "vOTING_PERIOD_OVER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal that is already ended"

------------------------------------------------
-- Error causes by bounded value
------------------------------------------------

type instance ErrorArg "oUT_OF_BOUND_VOTING_PERIOD" = ()

instance CustomErrorHasDoc "oUT_OF_BOUND_VOTING_PERIOD" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to set voting period that is out of bound."

type instance ErrorArg "oUT_OF_BOUND_QUORUM_THRESHOLD" = ()

instance CustomErrorHasDoc "oUT_OF_BOUND_QUORUM_THRESHOLD" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to set quorum threshold that is out of bound"

type instance ErrorArg "mAX_PROPOSALS_REACHED" = ()

instance CustomErrorHasDoc "mAX_PROPOSALS_REACHED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal when proposals max amount is already reached"

type instance ErrorArg "mAX_VOTES_REACHED" = ()

instance CustomErrorHasDoc "mAX_VOTES_REACHED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal when the votes max amount of that proposal is already reached"

type instance ErrorArg "pROPOSER_NOT_EXIST_IN_LEDGER" = ()

instance CustomErrorHasDoc "pROPOSER_NOT_EXIST_IN_LEDGER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Expect a proposer address to exist in Ledger but it is not found (Impossible Case)"

type instance ErrorArg "pROPOSAL_NOT_UNIQUE" = ()

instance CustomErrorHasDoc "pROPOSAL_NOT_UNIQUE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal that is already existed in the Storage."

type instance ErrorArg "fAIL_TRANSFER_CONTRACT_TOKENS" = ()

instance CustomErrorHasDoc "fAIL_TRANSFER_CONTRACT_TOKENS" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to cross-transfer BaseDAO tokens to another contract that does not exist or is not a valid FA2 contract."

type instance ErrorArg "mISSIGNED" = Packed (DataToSign SomeType)

instance CustomErrorHasDoc "mISSIGNED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Invalid signature provided."

type instance ErrorArg "bAD_ENTRYPOINT_PARAMETER" = ()

instance CustomErrorHasDoc "bAD_ENTRYPOINT_PARAMETER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Value passed to the entrypoint is not valid"

type instance ErrorArg "fAIL_DROP_PROPOSAL_NOT_OVER" = ()

instance CustomErrorHasDoc "fAIL_DROP_PROPOSAL_NOT_OVER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "An error occurred why trying to drop a proposal due to the proposal's voting period is not over"

type instance ErrorArg "fAIL_DROP_PROPOSAL_NOT_ACCEPTED" = ()

instance CustomErrorHasDoc "fAIL_DROP_PROPOSAL_NOT_ACCEPTED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "An error occurred why trying to drop a proposal due to the proposal is not an accepted proposal"

------------------------------------------------
-- Instances
------------------------------------------------

--customGeneric "Parameter" leftBalanced

instance ( HasAnnotation pm, NiceParameter pm
         , ParameterDeclaresEntrypoints op
         , RequireAllUniqueEntrypoints (Parameter pm op)
         ) =>
         ParameterHasEntrypoints (Parameter pm op) where
  type ParameterEntrypointsDerivation (Parameter pm op) = EpdDelegate

deriving anyclass instance
  (WellTypedIsoValue pm, WellTypedIsoValue op) =>
  IsoValue (Parameter pm op)
