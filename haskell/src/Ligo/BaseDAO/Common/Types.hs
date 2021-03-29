-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Common types between different DAO implementations.
module Ligo.BaseDAO.Common.Types
  ( TokenTransfer (..)
  , TransferType (..)
  , XtzTransfer (..)
  ) where

import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Ligo.BaseDAO.Types

--------------------------------------------------------------------------------
-- Transfer
--------------------------------------------------------------------------------

data TransferType
  = Xtz_transfer_type XtzTransfer
  | Token_transfer_type TokenTransfer
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

instance HasAnnotation TokenTransfer where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc TokenTransfer where
  typeDocMdDescription = "Describe a proposal which wants to transfer any FA2 tokens \
  \ to a certain address."
