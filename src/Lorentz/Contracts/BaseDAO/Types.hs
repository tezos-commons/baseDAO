-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.Types
  ( Operators
  , Ledger
  , Storage (..)

  , emptyStorage
  ) where

import qualified Data.Map.Internal as Map
import Text.Show (Show (..))

import Lorentz
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Runtime.GState (genesisAddress)

type Operators = BigMap (Address, (Address, TokenId)) ()

type Ledger = BigMap (Address, TokenId) Natural

-- | Storage of the FA2 contract
data Storage = Storage
  { sLedger       :: Ledger
  , sOperators    :: Operators
  , sTokenAddress :: Address
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

emptyStorage :: Storage
emptyStorage = Storage
  { sLedger =  BigMap $ Map.empty
  , sOperators = BigMap $ Map.empty
  -- TODO: make it configurable
  , sTokenAddress = genesisAddress
  }

instance TypeHasDoc Storage where
  typeDocMdDescription =
    "Contract's storage holding a big_map with all balances and the operators."
