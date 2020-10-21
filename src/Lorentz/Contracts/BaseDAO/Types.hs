-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.BaseDAO.Types
  ( Operators
  , Ledger
  , Parameter (..)
  , Storage (..)
  , StorageC
  , TransferOwnershipParam
  , MigrateParam
  , MigrationStatus

  , emptyStorage
  , mkStorage
  ) where

import qualified Data.Map.Internal as Map
import Text.Show (Show(..))

import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Michelson.Runtime.GState (genesisAddress)

type Operators = BigMap (Address, Address) ()

type Ledger = BigMap (Address, FA2.TokenId) Natural

-- | Migration status of the contract
data MigrationStatus
  = NotInMigration
  | MigratingTo Address
  | MigratedTo Address
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc MigrationStatus where
  typeDocMdDescription =
    "Migration status of the contract"

-- | Storage of the FA2 contract
data Storage = Storage
  { sLedger          :: Ledger
  , sOperators       :: Operators
  , sTokenAddress    :: Address
  , sAdmin           :: Address
  , sPendingOwner    :: Maybe Address
  , sMigrationStatus :: MigrationStatus
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Storage where
  typeDocMdDescription =
    "Contract's storage holding a big_map with all balances and the operators."

instance HasFieldOfType Storage name field =>
         StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

type StorageC store =
  StorageContains store
    [ "sAdmin" := Address
    , "sPendingOwner" := Maybe Address
    , "sLedger" := Ledger
    , "sMigrationStatus" := MigrationStatus
    ]

-- | Parameter of the BaseDAO contract
data Parameter
  = Call_FA2 FA2.Parameter
  | Transfer_ownership TransferOwnershipParam
  | Accept_ownership ()
  | Migrate MigrateParam
  | Confirm_migration ()
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate

type TransferOwnershipParam = ("newOwner" :! Address)
type MigrateParam = ("newAddress" :! Address)

emptyStorage :: Storage
emptyStorage = Storage
  { sLedger =  BigMap $ Map.empty
  , sOperators = BigMap $ Map.empty
  -- TODO: make it configurable
  , sTokenAddress = genesisAddress
  , sPendingOwner = Nothing
  , sAdmin = genesisAddress
  , sMigrationStatus = NotInMigration
  }

mkStorage
  :: Address
  -> Map (Address, FA2.TokenId) Natural
  -> Operators
  -> Storage
mkStorage admin balances operators = Storage
  { sLedger = BigMap balances
  , sOperators = operators
  , sTokenAddress = genesisAddress
  , sPendingOwner = Nothing
  , sAdmin = admin
  , sMigrationStatus = NotInMigration
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

-- Errors

type instance ErrorArg "nO_PENDING_ADMINISTRATOR_SET" = ()

instance CustomErrorHasDoc "nO_PENDING_ADMINISTRATOR_SET" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Received an `accept_ownership` call when no pending owner was set"

type instance ErrorArg "nOT_PENDING_ADMINISTRATOR" = ()

instance CustomErrorHasDoc "nOT_PENDING_ADMINISTRATOR" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an `accept_ownership` from an address other than what is in the pending owner field"

type instance ErrorArg "nOT_ADMINISTRATOR" = ()

instance CustomErrorHasDoc "nOT_ADMINISTRATOR" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an operation that require administrative privileges\
    \ from an address that is not the current administrator"

type instance ErrorArg "mIGRATED" = Address

instance CustomErrorHasDoc "mIGRATED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Recieved a call on a migrated contract"

type instance ErrorArg "nOT_MIGRATING" = ()

instance CustomErrorHasDoc "nOT_MIGRATING" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Recieved a confirm_migration call on a contract that is not in migration"

type instance ErrorArg "nOT_MIGRATION_TARGET" = ()

instance CustomErrorHasDoc "nOT_MIGRATION_TARGET" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Recieved a confirm_migration call on a contract from an address other than the new version"

type instance ErrorArg "fORBIDDEN_XTZ" = ()

instance CustomErrorHasDoc "fORBIDDEN_XTZ" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received some XTZ as part of a contract call, which is forbidden"
