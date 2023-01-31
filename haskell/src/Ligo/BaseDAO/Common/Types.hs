-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Common types between different DAO implementations.
module Ligo.BaseDAO.Common.Types
  ( LegacyTokenTransfer (..)
  , TokenTransfer (..)
  , TransferType (..)
  , XtzTransfer (..)
  ) where

import Lorentz
import Lorentz.Contracts.Spec.AbstractLedgerInterface qualified as FA12
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2

import Ligo.BaseDAO.Types

--------------------------------------------------------------------------------
-- Transfer
--------------------------------------------------------------------------------

data TransferType
  = Xtz_transfer_type XtzTransfer
  | Token_transfer_type TokenTransfer
  | Legacy_token_transfer_type LegacyTokenTransfer
  deriving stock Generic
  deriving anyclass IsoValue

instance HasAnnotation TransferType where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc TransferType where
  typeDocMdDescription = "Describe the transfer type of the proposal which are: \
  \token transfer type and xtz transfer type."

data XtzTransfer = XtzTransfer
  { xtAmount :: Mutez
  , xtRecipient :: Address
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation XtzTransfer where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc XtzTransfer where
  typeDocMdDescription = "Describe a proposal which wants to \
    \transfer some amount of XTZ to a certain address."

data TokenTransfer = TokenTransfer
  { ttContractAddress :: Address
  , ttTransferList :: [FA2.TransferItem]
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

data LegacyTokenTransfer = LegacyTokenTransfer
  { lttContractAddress :: Address
  , lttTransfer :: FA12.TransferParams
  }
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance HasAnnotation TokenTransfer where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc TokenTransfer where
  typeDocMdDescription = "Describe a proposal which wants to transfer any FA2 tokens \
  \ to a certain address."

instance HasAnnotation LegacyTokenTransfer where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc LegacyTokenTransfer where
  typeDocMdDescription = "Describe a proposal which wants to transfer any FA1.2 tokens \
  \ to a certain address."
