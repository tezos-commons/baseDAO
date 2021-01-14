-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Ligo.BaseDAO.Helper
  ( defaultConfigL
  , dynRecUnsafe
  , mkFullStorage
  ) where

import Universum ((*))
import Lorentz

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Ligo.BaseDAO.Types

defaultConfigL :: ConfigL
defaultConfigL = ConfigL
  { cUnfrozenTokenMetadata = FA2.TokenMetadata
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
  , cCustomEntrypoints = mempty

  , cMaxVotingPeriod = 60 * 60 * 24 * 30
  , cMinVotingPeriod = 1 -- value between 1 second - 1 month

  , cMaxQuorumThreshold = 1000
  , cMinQuorumThreshold = 1

  , cMaxVotes = 1000
  , cMaxProposals = 500
  }

-- Helper
------------------------------------------------

-- | Construct 'DynamicRec' assuming it contains no mandatory entries.
dynRecUnsafe :: DynamicRec n
dynRecUnsafe = DynamicRec mempty

mkFullStorage
  :: ConfigL
  -> ContractExtraL
  -> Address
  -> Ledger
  -> Operators
  -> FullStorage
mkFullStorage config extra admin bal operators =
  let storage = StorageL
        { sAdmin = admin
        , sExtra = extra
        , sLedger = bal
        , sMetadata = #metadata mempty
        , sMigrationStatus = NotInMigration
        , sOperators = operators
        , sPendingOwner = admin -- TODO: placeholder for now
        , sPermitsCounter = Nonce 0
        , sProposals = mempty
        , sProposalKeyListSortByDate = mempty
        , sQuorumThreshold = 33
        , sVotingPeriod = 11
        , sTokenAddress = admin -- TODO: placeholder for now
        }

  in FullStorage
      { fsStorage = storage
      , fsConfig = config
      }
