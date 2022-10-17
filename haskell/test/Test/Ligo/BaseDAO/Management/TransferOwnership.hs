-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# LANGUAGE ApplicativeDo #-}

module Test.Ligo.BaseDAO.Management.TransferOwnership
  ( authenticateSender
  , bypassAcceptForSelf
  , changeToPendingAdmin
  , invalidatePendingOwner
  , noPendingAdmin
  , notSetAdmin
  , pendingOwnerNotTheSame
  , rewritePendingOwner
  , transferOwnership
  , transferOwnershipSetsPendingOwner
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Util.Named
import Morley.Util.SizedList (SizedList, SizedList'(..))
import Test.Cleveland

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

type WithOriginateFn num m =
     (SizedList num ImplicitAddress -> Storage)
  -> (SizedList num ImplicitAddress -> ContractHandle Parameter Storage () -> m ())
  -> m ()

type WithStorage = ImplicitAddress -> Storage

transferOwnership
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
transferOwnership withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::< _) -> initialStorage owner) $ \(_ ::< wallet1 ::< Nil') baseDao ->
    withSender wallet1 $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (toAddress wallet1)))
      & expectNotAdmin

transferOwnershipSetsPendingOwner
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
transferOwnershipSetsPendingOwner withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $ \(owner ::< wallet1 ::< Nil') baseDao -> do
    withSender owner $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (toAddress wallet1)))
    mNewPendingOwner <- sPendingOwnerRPC <$> getStorageRPC baseDao
    assert (mNewPendingOwner == (toAddress wallet1)) "Pending owner was not set as expected"

authenticateSender
  :: MonadCleveland caps m
  => WithOriginateFn 3 m -> WithStorage -> m ()
authenticateSender withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< wallet1 ::< wallet2 ::< Nil') baseDao -> do
      withSender owner $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (toAddress wallet1)))
      withSender wallet2 $ (transfer baseDao $ calling (Call @"Accept_ownership") ())
        & expectNotPendingOwner

changeToPendingAdmin
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
changeToPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< wallet1 ::< Nil') baseDao -> do
      withSender owner $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (toAddress wallet1)))
      withSender wallet1 $ (transfer baseDao $ calling (ep @"Accept_ownership") ())
      administrator <- sAdminRPC <$> (getStorageRPC baseDao)
      assert (administrator == (toAddress wallet1)) "Administrator was not set from pending owner"

noPendingAdmin
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
noPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(_ ::< wallet1 ::< Nil') baseDao -> do
      withSender wallet1 $
        (transfer baseDao $ calling (ep @"Accept_ownership") ())
        & expectNotPendingOwner

pendingOwnerNotTheSame
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
pendingOwnerNotTheSame withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< wallet1 ::< Nil') baseDao -> withSender owner $ do
      (transfer baseDao $ calling (ep @"Transfer_ownership")
        (#newOwner :! (toAddress wallet1)))
      (transfer baseDao $ calling (ep @"Accept_ownership") ())
      & expectNotPendingOwner

notSetAdmin
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
notSetAdmin withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< wallet1 ::< Nil') baseDao -> do
      withSender owner . inBatch $ do
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (toAddress wallet1)))
        -- Make the call once again to make sure the admin still retains admin
        -- privileges
        (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (toAddress wallet1)))
        pure ()

rewritePendingOwner
  :: MonadCleveland caps m
  => WithOriginateFn 3 m -> WithStorage -> m ()
rewritePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< wallet1 ::< wallet2 ::< Nil') baseDao -> do
      withSender owner . inBatch $ do
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (toAddress wallet1)))
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (toAddress wallet2)))
        pure ()
      pendingOwner <- sPendingOwnerRPC <$> (getStorageRPC baseDao)
      assert (pendingOwner == (toAddress wallet2)) "Pending owner from earlier call was not re-written"

invalidatePendingOwner
  :: MonadCleveland caps m
  => WithOriginateFn 2 m -> WithStorage -> m ()
invalidatePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< wallet1 ::< Nil') baseDao -> do
      withSender owner . inBatch $ do
          (transfer baseDao $ calling (ep @"Transfer_ownership")
            (#newOwner :! (toAddress wallet1)))
          (transfer baseDao $ calling (ep @"Transfer_ownership")
            (#newOwner :! (toAddress owner)))
          pure ()
      administrator <- sAdminRPC <$> (getStorageRPC baseDao)
      assert (administrator == (toAddress owner)) "Pending owner from earlier call was not re-written"

bypassAcceptForSelf
  :: MonadCleveland caps m
  => WithOriginateFn 1 m -> WithStorage -> m ()
bypassAcceptForSelf withOriginatedFn initialStorage =
  withOriginatedFn (\(owner::<_) -> initialStorage owner) $
    \(owner ::< Nil') baseDao -> do
      withSender owner $ do
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (toAddress $ chAddress baseDao)))
      currentAdmin <- sAdminRPC <$> getStorage @Storage (chAddress baseDao)
      assert (currentAdmin == (toAddress $ chAddress baseDao)) "Admin address was not set"

expectNotAdmin
  :: (MonadCleveland caps m)
  => m a -> m ()
expectNotAdmin = expectFailedWith notAdmin

expectNotPendingOwner
  :: (MonadCleveland caps m)
  => m a -> m ()
expectNotPendingOwner = expectFailedWith notPendingAdmin
