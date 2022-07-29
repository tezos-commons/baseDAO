-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.RegistryDAO.Types
  ( RegistryDaoProposalMetadata (..)
  , TransferProposal (..)
  , ConfigProposal (..)
  , UpdateReceiverParam (..)
  ) where

import Universum

import Lorentz as L

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types

-- | Helper type for unpack/pack
data RegistryDaoProposalMetadata
  = Configuration_proposal ConfigProposal
  | Transfer_proposal TransferProposal
  | Update_receivers_proposal UpdateReceiverParam
  | Update_guardian Address
  | Update_contract_delegate (Maybe KeyHash)

data UpdateReceiverParam
  = Add_receivers [Address]
  | Remove_receivers [Address]

data TransferProposal = TransferProposal
  { tpAgoraPostId  :: Natural
  , tpTransfers    :: [TransferType]
  , tpRegistryDiff :: [(MText, Maybe MText)]
  }

data ConfigProposal = ConfigProposal
  { cpFrozenScaleValue :: Maybe Natural
  , cpFrozenExtraValue :: Maybe Natural
  , cpSlashScaleValue :: Maybe Natural
  , cpSlashDivisionValue :: Maybe Natural
  , cpMaxProposalSize :: Maybe Natural
  }

customGeneric "ConfigProposal" ligoLayout
deriving anyclass instance IsoValue ConfigProposal

customGeneric "TransferProposal" ligoLayout
deriving anyclass instance IsoValue TransferProposal

customGeneric "UpdateReceiverParam" ligoLayout
deriving anyclass instance IsoValue UpdateReceiverParam

customGeneric "RegistryDaoProposalMetadata" ligoLayout
deriving anyclass instance IsoValue RegistryDaoProposalMetadata

instance HasAnnotation RegistryDaoProposalMetadata where
  annOptions = baseDaoAnnOptions

instance HasAnnotation UpdateReceiverParam where
  annOptions = baseDaoAnnOptions

instance HasAnnotation TransferProposal where
  annOptions = baseDaoAnnOptions

instance HasAnnotation ConfigProposal where
  annOptions = baseDaoAnnOptions
