-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.Management
  ( acceptOwnership
  , confirmMigration
  , ensureNotMigrated
  , migrate
  , transferOwnership
  , authorizeAdmin
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Doc (transferOwnershipDoc, acceptOwnershipDoc, migrateDoc, confirmMigrationDoc)
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.ManagedLedger.Doc (DRequireRole(..))

-- | Set the value of pending owner in storage
-- using the address in parameter.
transferOwnership
  :: IsoValue pm => Entrypoint' TransferOwnershipParam (Storage pm) s
transferOwnership = do
  doc $ DDescription transferOwnershipDoc
  dip $ do
    ensureNotMigrated
    authorizeAdmin

  fromNamed #newOwner
  stSetField #sPendingOwner

  nil; pair

-- | Checks if pending owner is set and set the value of new administrator
acceptOwnership
  :: IsoValue pm => Entrypoint' () (Storage pm) s
acceptOwnership = do
  doc $ DDescription acceptOwnershipDoc
  drop @()
  ensureNotMigrated

  authorizePendingOwner

  stGetField #sPendingOwner
  stSetField #sAdmin

  nil; pair

-- Authorises admin and set the migration status using the new address
-- in param.
migrate :: IsoValue pm => Entrypoint' MigrateParam (Storage pm) s
migrate = do
  doc $ DDescription migrateDoc
  dip $ do
    ensureNotMigrated
    authorizeAdmin
  fromNamed #newAddress
  wrap_ @MigrationStatus #cMigratingTo
  stSetField #sMigrationStatus
  nil; pair

-- Authorise sender and move pending owner address to stack top.
ensureNotMigrated ::
  StorageC store pm => store : s :-> store : s
ensureNotMigrated = do
  stGetField #sMigrationStatus
  caseT @MigrationStatus
    ( #cNotInMigration /-> nop
    , #cMigratingTo /-> drop
    , #cMigratedTo /-> failCustom #mIGRATED
    )

-- Confirm that the sender is the new contract address and set `MIGRATED_TO` status
confirmMigration :: IsoValue pm => Entrypoint' () (Storage pm) s
confirmMigration = do
  doc $ DDescription confirmMigrationDoc
  drop
  doc $ DRequireRole "migration target contract"
  stGetField #sMigrationStatus
  caseT @MigrationStatus
    ( #cNotInMigration /-> failCustom_ #nOT_MIGRATING
    , #cMigratingTo /-> do
        dup
        sender
        if IsEq then nop else failCustom_ #nOT_MIGRATION_TARGET
        wrap_ @MigrationStatus #cMigratedTo
        stSetField #sMigrationStatus
    , #cMigratedTo /-> failCustom #mIGRATED
    )
  nil; pair

-- Authorise sender and move pending owner address to stack top.
authorizePendingOwner ::
  StorageC store pm => store : s :-> store : s
authorizePendingOwner = do
  doc $ DRequireRole "pending owner"
  stGetField #sPendingOwner
  sender
  if IsEq then nop else failCustom_ #nOT_PENDING_ADMIN

-- Authorise administrator.
authorizeAdmin ::
  StorageC store pm => store : s :-> store : s
authorizeAdmin = do
  doc $ DRequireRole "administrator"
  stGetField #sAdmin; sender;
  if IsEq then nop else failCustom_ #nOT_ADMIN
