-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

-- | TreasuryDAO Types
module Lorentz.Contracts.TreasuryDAO.Types
  ( AgoraPostId (..)
  , ConfigProposal (..)
  , NormalProposal (..)
  , TokenTransfer (..)
  , TransferType (..)
  , TreasuryDaoContractExtra (..)
  , TreasuryDaoCustomEntrypointsKind
  , TreasuryDaoExtraInterface (..)
  , TreasuryDaoProposalMetadata (..)
  , XtzTransfer (..)
  ) where

import Lorentz

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

-- | TreasuryDAO has mainly configuration fields.
data TreasuryDaoContractExtra = TreasuryDaoContractExtra
  { ceFrozenScaleValue :: Natural -- a
  , ceFrozenExtraValue :: Natural -- b
  , ceSlashScaleValue :: Natural -- c
  , ceSlashDivisionValue :: Natural -- d
  , ceMinXtzAmount :: Mutez -- y
  , ceMaxXtzAmount :: Mutez -- z
  , ceMaxProposalSize :: Natural --z
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation TreasuryDaoContractExtra where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc TreasuryDaoContractExtra where
  typeDocMdDescription = "Describe TreasuryDAO's extra fields in storage. \
    \They are mainly configuration fields."

instance Default TreasuryDaoContractExtra where
  def = TreasuryDaoContractExtra
    { ceFrozenScaleValue = 1 -- a
    , ceFrozenExtraValue = 0 -- b
    , ceSlashScaleValue = 1 -- c
    , ceSlashDivisionValue = 1 -- d
    , ceMinXtzAmount = toMutez 1 -- y
    , ceMaxXtzAmount = toMutez 10000 -- z
    , ceMaxProposalSize = 1000 --z
    }

-- | Proposal metadata used in Treasuy DAO
data TreasuryDaoProposalMetadata
  = NormalProposalType NormalProposal
  | ConfigProposalType ConfigProposal
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation TreasuryDaoProposalMetadata where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc TreasuryDaoProposalMetadata where
  typeDocMdDescription = "Describe the metadata of a proposal in Treasury DAO. In TreasuryDAO, \
    \there are 2 types of proposal, a configuration proposal and a treasury proposal. A treasuy \
    \proposal can contains a list of transfers which divided into 2 types: token transfers and \
    \xtz transfers."

-- | A Treasury DAO proposal, contains list of transfers of 2 types:
-- Token transfer and Xtz transfer
data NormalProposal = NormalProposal
  { npAgoraPostId :: AgoraPostId
  , npTransfers :: [TransferType]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation NormalProposal where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc NormalProposal where
  typeDocMdDescription =
    "A treasury proposal which contains an Agora post ID and a list of transfer items."

newtype AgoraPostId = AgoraPostId Natural
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation AgoraPostId where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc AgoraPostId where
  typeDocMdDescription = "Describe an Agora post ID."

data TransferType
  = XtzTransferType XtzTransfer
  | TokenTransferType TokenTransfer
  deriving stock Generic
  deriving anyclass IsoValue

instance HasAnnotation TransferType where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc TransferType where
  typeDocMdDescription = "Describe the transfer type of the Treasury proposal which are: \
  \token transfer type and xtz transfer type."


data XtzTransfer = XtzTransfer
  { xtAmount :: Mutez
  , xtRecipient :: Address
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation XtzTransfer where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc XtzTransfer where
  typeDocMdDescription = "Describe a proposal which wants to \
    \transfer some amount of XTZ to a certain address."


data TokenTransfer = TokenTransfer
  { ttContractAddress :: Address
  , ttTransferList :: [FA2.TransferItem]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation TokenTransfer where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc TokenTransfer where
  typeDocMdDescription = "Describe a proposal which wants to transfer any FA2 tokens \
  \ to a certain address."


-- | Special proposal that allow updating certain scale values in 'contractExtra'
data ConfigProposal = ConfigProposal
  { cpFrozenScaleValue :: Maybe Natural -- a
  , cpFrozenExtraValue :: Maybe Natural -- b
  , cpSlashScaleValue :: Maybe Natural -- c
  , cpSlashDivisionValue :: Maybe Natural -- d
  , cpMinXtzAmount :: Maybe Mutez -- y
  , cpMaxXtzAmount :: Maybe Mutez -- z
  , cpMaxProposalSize :: Maybe Natural --z
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation ConfigProposal where
  annOptions = DAO.baseDaoAnnOptions

instance TypeHasDoc ConfigProposal where
  typeDocMdDescription =
    "Describe a configuration proposal in Registry DAO. It is used to \
    \update the configuration values in contract extra that affect the process of \
    \checking if a proposal is valid or not and how tokens are slash when a proposal \
    \is rejected."


data TreasuryDaoExtraInterface
  = Default
  | None -- Not needed
  deriving stock (Generic)
  deriving anyclass (IsoValue, HasAnnotation)


instance TypeHasDoc TreasuryDaoExtraInterface where
  typeDocMdDescription = "Additional entrypoints of Game DAO."

instance ParameterHasEntrypoints TreasuryDaoExtraInterface where
  type ParameterEntrypointsDerivation TreasuryDaoExtraInterface = EpdPlain

data TreasuryDaoCustomEntrypointsKind
instance EntrypointKindHasDoc TreasuryDaoCustomEntrypointsKind where
  entrypointKindPos = 1065
  entrypointKindSectionName = "TreasuryDAO custom entrypoints"
  entrypointKindSectionDescription = Just
    "Some functionality specific to Treasury DAO. \
    \This demonstrates that we can e.g. add a `%default` entrypoint used to send \
    \mutez to the contract."


type instance ErrorArg "fAIL_DECISION_LAMBDA" = ()

instance CustomErrorHasDoc "fAIL_DECISION_LAMBDA" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to execute decision lambda but result in errors."
