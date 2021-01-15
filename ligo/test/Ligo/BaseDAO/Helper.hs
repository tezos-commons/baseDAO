-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Ligo.BaseDAO.Helper
  ( defaultConfigL
  , dynRecUnsafe
  , mkFullStorage
  ) where

import Ligo.BaseDAO.Types
import Lorentz
import Universum ((*))

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

defaultConfigL :: ConfigL
defaultConfigL  = ConfigL
  { cUnfrozenTokenMetadata =
      FA2.mkTokenMetadata "unfrozen_token" "Unfrozen Token" "8"
  , cFrozenTokenMetadata =
      FA2.mkTokenMetadata "frozen_token" "Frozen Token" "8"
  , cProposalCheck = do
      dropN @2; push True
  , cRejectedProposalReturnValue = do
      dropN @2; push (0 :: Natural); toNamed #slash_amount
  , cDecisionLambda = do
      drop; nil
  , cCustomEntrypoints = dynRecUnsafe

  , cMaxVotingPeriod = 60 * 60 * 24 * 30
  , cMinVotingPeriod = 1 -- value between 1 second - 1 month

  , cMaxQuorumThreshold = 1000
  , cMinQuorumThreshold = 1

  , cMaxVotes = 1000
  , cMaxProposals = 500
  }

-- Helper
------------------------------------------------

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
