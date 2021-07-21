-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.TreasuryDAO.Types
  ( TransferProposal (..)
  , TreasuryDaoProposalMetadata (..)
  ) where

import Universum

import Lorentz as L

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data TransferProposal = TransferProposal
  { tpAgoraPostId :: Natural
  , tpTransfers :: [TransferType]
  }

instance HasAnnotation TransferProposal where
  annOptions = baseDaoAnnOptions

customGeneric "TransferProposal" ligoLayout

deriving anyclass instance IsoValue TransferProposal

-- | Helper type for unpack/pack
data TreasuryDaoProposalMetadata
  = Transfer_proposal TransferProposal
  | Update_guardian Address

instance HasAnnotation TreasuryDaoProposalMetadata where
  annOptions = baseDaoAnnOptions

customGeneric "TreasuryDaoProposalMetadata" ligoLayout
deriving anyclass instance IsoValue TreasuryDaoProposalMetadata