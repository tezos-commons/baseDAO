-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simple DAO for a Moba-like game.
module Lorentz.Contracts.GameDAO
  ( GameDaoProposalMetadata (..)
  , Storage
  , Parameter
  , ProposalType (..)
  , BalanceChange (..)
  , ItemChange (..)
  , HeroChange (..)
  , NewContents
  , NewContent (..)

  , gameDaoContract
  , config
  ) where

import Lorentz

import qualified Lorentz.Contracts.BaseDAO as DAO
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Universum hiding (drop, swap, (>>))
import Util.Markdown

-- | Global metadata of the contract.
data GameDaoContractExtra = GameDaoContractExtra
  { ceAcceptedProposalsNum :: Natural
    -- ^ Counts number of ever accepted proposals.
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation GameDaoContractExtra where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc GameDaoContractExtra where
  typeDocMdDescription =
    "As part of contract global state this carries nothing"

instance Default GameDaoContractExtra where
  def = GameDaoContractExtra 0

-- | Metadata we keep for each proposal.
data GameDaoProposalMetadata = GameDaoProposalMetadata
  { pmProposalType :: ProposalType
    -- ^ Proposal type.
  , pmProposalDescription :: MText
    -- ^ Description of the proposal in free form.
  , pmConsumerAddr :: Address
    -- ^ Address at which the report on proposal acceptance.
  }
  deriving stock (Generic)
  deriving anyclass IsoValue

instance HasAnnotation GameDaoProposalMetadata where
  annOptions = DAO.baseDaoAnnOptions

-- | Extra entrypoints supported by our DAO.
data GameDaoExtraInterface
  = Default
  | ResetProposalsCounter
  deriving stock (Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc GameDaoExtraInterface where
  typeDocMdDescription = "Additional entrypoints of Game DAO."

instance ParameterHasEntrypoints GameDaoExtraInterface where
  type ParameterEntrypointsDerivation GameDaoExtraInterface = EpdPlain

data ProposalType = BalanceType BalanceChange | NewType NewContents
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation ProposalType where
  annOptions = defaultAnnOptions { fieldAnnModifier = toSnake }

instance TypeHasDoc ProposalType where
  typeDocMdDescription = "There are 2 types of proposal that Game DAO supports. \
    \'balance_type' is for small update to existing mechanic, items and heroes. \
    \'new_type' is for proposing new contents all together."

data BalanceChange = BalanceChange
  { bcItemChanges :: [ItemChange]
  , bcHeroChanges :: [HeroChange]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation BalanceChange where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc BalanceChange where
  typeDocMdDescription = "Describe the update of all items and heroes in the proposal."

data ItemChange = ItemChange
  { icItemName :: MText
  , icChangelogs :: [MText]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance TypeHasDoc ItemChange where
  typeDocMdDescription = "Describe the update of a specific item."

instance HasAnnotation ItemChange where
  annOptions = DAO.baseDaoAnnOptions

data HeroChange = HeroChange
  { hcHeroName :: MText
  , hcChangelogs :: [MText]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance TypeHasDoc HeroChange where
  typeDocMdDescription = "Describe the update of a specific hero."

instance HasAnnotation HeroChange where
  annOptions = DAO.baseDaoAnnOptions

type NewContents = [NewContent]

data NewContent = NewContent
  { ncContentName :: MText
  , ncContentDescription :: [MText]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance TypeHasDoc NewContent where
  typeDocMdDescription = "Describe a new content. The content should have a \
    \name and detail description."

instance HasAnnotation NewContent where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc GameDaoProposalMetadata where
  typeDocMdDescription = "GameDAO's metadata. This fields affect how proposal \
    \got accepted and how many tokens will be frozen"

data GameDaoCustomEntrypointsKind
instance EntrypointKindHasDoc GameDaoCustomEntrypointsKind where
  entrypointKindPos = 1065
  entrypointKindSectionName = "GameDAO custom entrypoints"
  entrypointKindSectionDescription = Just
    "Some functionality specific to Game DAO. \
    \This demonstrates that we can e.g. add a `%default` entrypoint used to send \
    \mutez to the contract."

----------------------------------------------------------------------------

type instance ErrorArg "fAIL_DECISION_LAMBDA" = ()

instance CustomErrorHasDoc "fAIL_DECISION_LAMBDA" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to execute decision lambda but result in errors."

----------------------------------------------------------------------------

type Parameter = DAO.Parameter GameDaoProposalMetadata GameDaoExtraInterface
type Storage = DAO.Storage GameDaoContractExtra GameDaoProposalMetadata

-- | For "balance change", the proposer needs to freeze 10 tokens
-- For "new content", the proposal needs to freeze 50 tokens
gameDaoProposalCheck
  :: forall s store. (DAO.ProposeParams GameDaoProposalMetadata) : store : s
  :-> Bool : s
gameDaoProposalCheck = do
  dip drop
  getFieldNamed #ppFrozenToken; swap
  toField #ppProposalMetadata
  toField #pmProposalType
  stackType @(ProposalType : "ppFrozenToken" :! Natural : s)
  caseT
    ( #cBalanceType /-> do drop; push (10 :: Natural)
    , #cNewType /-> do drop; push (50 :: Natural)
    )
  toNamed #requireValue

  if #requireValue <=. #ppFrozenToken then
    push True
  else
    push False

-- | In case of a proposal got rejected, the proposer loses half of the tokens.
gameDaoRejectedProposalReturnValue
  :: forall s store. (DAO.Proposal GameDaoProposalMetadata) : store : s
  :-> ("slash_amount" :! Natural) : s
gameDaoRejectedProposalReturnValue = do
  dip drop
  toField #pProposerFrozenToken
  push (2 :: Natural)
  swap
  ediv
  ifSome car $
    push (0 :: Natural)
  toNamed #slash_amount

-- | When a proposal got accepted, call a consumer contract with the proposal
-- description. It would be more useful to send the whole proposal itself but
-- using just the description is simpler to test.
decisionLambda
  ::  forall s store. (DAO.StorageC store GameDaoContractExtra GameDaoProposalMetadata)
  =>  (DAO.Proposal GameDaoProposalMetadata) : store : s
  :-> List Operation : store : s
decisionLambda = do
  toField #pMetadata
  getField #pmProposalDescription

  dip $ do
    toField #pmConsumerAddr
    contractCallingUnsafe @MText DefEpName
    ifSome nop $ do
      failCustom_ #fAIL_DECISION_LAMBDA
    push zeroMutez
  transferTokens
  nil; swap; cons

  dip $ do
    stGetField #sExtra
    getField #ceAcceptedProposalsNum
    push @Natural 1; add
    setField #ceAcceptedProposalsNum
    stSetField #sExtra

extraEntrypoints
  :: forall s store. (DAO.StorageC store GameDaoContractExtra GameDaoProposalMetadata)
  => GameDaoExtraInterface : store : s :-> List Operation : store : s
extraEntrypoints =
  entryCase (Proxy @GameDaoCustomEntrypointsKind)
    ( #cDefault /-> do
        doc $ DDescription "Entrypoint used for transferring money to the contract."
        nil
    , #cResetProposalsCounter /-> do
        doc $ DDescription "An entrypoint that performs something smart (or not)."
        stGetField #sExtra
        push 0
        setField #ceAcceptedProposalsNum
        stSetField #sExtra
        nil
    )

config :: DAO.Config GameDaoContractExtra GameDaoProposalMetadata GameDaoExtraInterface
config = DAO.defaultConfig
  { DAO.cDaoName = "Game DAO"
  , DAO.cDaoDescription = [md|A simple DAO for a Moba-like game.|]

  , DAO.cProposalCheck = gameDaoProposalCheck
  , DAO.cRejectedProposalReturnValue = gameDaoRejectedProposalReturnValue
  , DAO.cDecisionLambda = decisionLambda
  , DAO.cCustomCall = extraEntrypoints

  , DAO.cMaxVotingPeriod = 60 * 60 * 24 * 30 -- 1 months
  , DAO.cMinVotingPeriod = 1 -- value between 1 second - 1 month

  , DAO.cMaxQuorumThreshold = 1000
  , DAO.cMinQuorumThreshold = 1

  , DAO.cMaxVotes = 1000
  , DAO.cMaxProposals = 500
  }

gameDaoContract :: Contract Parameter Storage
gameDaoContract = DAO.baseDaoContract config
