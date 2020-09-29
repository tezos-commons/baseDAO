-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.BaseDAO.Types
  ( Operators
  , Ledger
  , Storage (..)

  , emptyStorage
  , mkStorage
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

  , sAdmin        :: Address
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Storage where
  typeDocMdDescription =
    "Contract's storage holding a big_map with all balances and the operators."

emptyStorage :: Storage
emptyStorage = Storage
  { sLedger =  BigMap $ Map.empty
  , sOperators = BigMap $ Map.empty
  -- TODO: make it configurable
  , sTokenAddress = genesisAddress
  , sAdmin = genesisAddress
  }

mkStorage
  :: Address
  -> Map (Address, TokenId) Natural
  -> Operators
  -> Storage
mkStorage admin balances operators = Storage
  { sLedger = BigMap balances
  , sOperators = operators
  , sTokenAddress = genesisAddress
  , sAdmin = admin
  }

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
