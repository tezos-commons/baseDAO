-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to the
-- `withOriginatedFn` function

module BaseDAO.ShareTest.Management
  ( transferOwnership
  , setPendingOwner
  , notSetAdmin
  , rewritePendingOwner
  , invalidatePendingOwner
  , respectMigratedState
  , authenticateSender
  , changeToPendingAdmin
  , noPendingAdmin
  , pendingOwnerNotTheSame
  , acceptOwnerRespectMigration
  , migrationAuthenticateSender
  , migrationSetPendingOwner
  , migrationOverwritePrevious
  , migrationAllowCallUntilConfirm
  , confirmMigAuthenticateSender
  , confirmMigAuthenticateState
  , confirmMigFinalize
  , expectNotAdmin
  , expectNotPendingOwner
  , expectNotMigrating
  , expectNotMigrationTarget
  , expectMigrated
  , expectForbiddenXTZ
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest
import Util.Named

import BaseDAO.ShareTest.Common
import qualified Lorentz.Contracts.BaseDAO.Types as DAO

type WithOriginateFn m param st = Integer
  -> ([Address] -> st)
  -> ([Address] -> TAddress param  -> m ())
  -> m ()

type WithStorage st = Address -> st

transferOwnership
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
transferOwnership withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
    callFrom (AddressResolved wallet1) baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
      & expectNotAdmin (toAddress baseDao)

setPendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
setPendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
        callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()

notSetAdmin
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
notSetAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      -- Make the call once again to make sure the admin still retains admin
      -- privileges
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)

rewritePendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
rewritePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet2)
      -- Make the accept ownership call from wallet1 and see that it fails
      callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner (toAddress baseDao)
      -- Make the accept ownership call from wallet1 and see that it works
      callFrom (AddressResolved wallet2) baseDao (Call @"Accept_ownership") ()

invalidatePendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
invalidatePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! owner)
      -- Make the accept ownership call from wallet1 and see that it fails
      -- with 'not pending owner' error
      callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner (toAddress baseDao)

respectMigratedState
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
respectMigratedState withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, newOwner] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! newOwner)
        & expectMigrated newAddress1

authenticateSender
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
authenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      callFrom (AddressResolved wallet2) baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner (toAddress baseDao)

changeToPendingAdmin
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
changeToPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()

noPendingAdmin
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
noPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, wallet1] baseDao -> do
      callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
      & expectNotPendingOwner (toAddress baseDao)

pendingOwnerNotTheSame
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
pendingOwnerNotTheSame withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      callFrom (AddressResolved owner) baseDao (Call @"Accept_ownership") ()
      & expectNotPendingOwner (toAddress baseDao)

acceptOwnerRespectMigration
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
acceptOwnerRespectMigration withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
      callFrom (AddressResolved owner) baseDao (Call @"Accept_ownership") ()
        & expectMigrated newAddress1


migrationAuthenticateSender
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationAuthenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[_, newAddress1, randomAddress] baseDao -> do
      callFrom (AddressResolved randomAddress) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        & expectNotAdmin (toAddress baseDao)

migrationSetPendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationSetPendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()

migrationOverwritePrevious
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationOverwritePrevious withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, newAddress2] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress2)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      callFrom (AddressResolved newAddress2) baseDao (Call @"Confirm_migration") ()

migrationAllowCallUntilConfirm
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationAllowCallUntilConfirm withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, newAddress2] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress2)

confirmMigAuthenticateSender
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
confirmMigAuthenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, randomAddress] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      callFrom (AddressResolved randomAddress) baseDao (Call @"Confirm_migration") ()
        & expectNotMigrationTarget (toAddress baseDao)

confirmMigAuthenticateState
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
confirmMigAuthenticateState withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, newAddress1] baseDao -> do
      callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
        & expectNotMigrating (toAddress baseDao)

confirmMigFinalize
  :: forall caps base m param pm st
  . (MonadNettest caps base m, DAO.ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
confirmMigFinalize withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1] baseDao -> do
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
      callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        & expectMigrated newAddress1

expectNotAdmin
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectNotAdmin addr = expectFailed addr [mt|NOT_ADMIN|]

expectNotPendingOwner
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectNotPendingOwner addr = expectFailed addr [mt|NOT_PENDING_ADMIN|]

expectNotMigrating
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectNotMigrating addr = expectFailed addr [mt|NOT_MIGRATING|]

expectNotMigrationTarget
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectNotMigrationTarget addr = expectFailed addr [mt|NOT_MIGRATION_TARGET|]

expectMigrated
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectMigrated addr = expectCustomError #mIGRATED addr

expectForbiddenXTZ
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectForbiddenXTZ addr = expectFailed addr [mt|FORBIDDEN_XTZ|]
