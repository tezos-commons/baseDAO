-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to the
-- `withOriginated` function

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
import Morley.Tezos.Address (ConstrainedAddress(MkAddress))
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

type WithOriginateFn m = Integer
  -> ([ImplicitAddress] -> Storage)
  -> ([ImplicitAddress] -> ContractHandle Parameter Storage () -> m ())
  -> m ()

type WithStorage = ImplicitAddress -> Storage

transferOwnership
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
transferOwnership withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
    withSender wallet1 $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (MkAddress wallet1)))
      & expectNotAdmin

transferOwnershipSetsPendingOwner
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
transferOwnershipSetsPendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[owner, wallet1] baseDao -> do
    withSender owner $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (MkAddress wallet1)))
    mNewPendingOwner <- sPendingOwnerRPC <$> getStorageRPC baseDao
    assert (mNewPendingOwner == (MkAddress wallet1)) "Pending owner was not set as expected"

authenticateSender
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
authenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (MkAddress wallet1)))
      withSender wallet2 $ (transfer baseDao $ calling (Call @"Accept_ownership") ())
        & expectNotPendingOwner

changeToPendingAdmin
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
changeToPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner $ (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (MkAddress wallet1)))
      withSender wallet1 $ (transfer baseDao $ calling (ep @"Accept_ownership") ())
      administrator <- sAdminRPC <$> (getStorageRPC baseDao)
      assert (administrator == (MkAddress wallet1)) "Administrator was not set from pending owner"

noPendingAdmin
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
noPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, wallet1] baseDao -> do
      withSender wallet1 $
        (transfer baseDao $ calling (ep @"Accept_ownership") ())
        & expectNotPendingOwner

pendingOwnerNotTheSame
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
pendingOwnerNotTheSame withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> withSender owner $ do
      (transfer baseDao $ calling (ep @"Transfer_ownership")
        (#newOwner :! (MkAddress wallet1)))
      (transfer baseDao $ calling (ep @"Accept_ownership") ())
      & expectNotPendingOwner

notSetAdmin
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
notSetAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner . inBatch $ do
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (MkAddress wallet1)))
        -- Make the call once again to make sure the admin still retains admin
        -- privileges
        (transfer baseDao $ calling (ep @"Transfer_ownership") (#newOwner :! (MkAddress wallet1)))
        pure ()

rewritePendingOwner
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
rewritePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner . inBatch $ do
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (MkAddress wallet1)))
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (MkAddress wallet2)))
        pure ()
      pendingOwner <- sPendingOwnerRPC <$> (getStorageRPC baseDao)
      assert (pendingOwner == (MkAddress wallet2)) "Pending owner from earlier call was not re-written"

invalidatePendingOwner
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
invalidatePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner . inBatch $ do
          (transfer baseDao $ calling (ep @"Transfer_ownership")
            (#newOwner :! (MkAddress wallet1)))
          (transfer baseDao $ calling (ep @"Transfer_ownership")
            (#newOwner :! (MkAddress owner)))
          pure ()
      administrator <- sAdminRPC <$> (getStorageRPC baseDao)
      assert (administrator == (MkAddress owner)) "Pending owner from earlier call was not re-written"

bypassAcceptForSelf
  :: MonadCleveland caps m
  => WithOriginateFn m -> WithStorage -> m ()
bypassAcceptForSelf withOriginatedFn initialStorage =
  withOriginatedFn 1 (\(owner:_) -> initialStorage owner) $
    \[owner] baseDao -> do
      withSender owner $ do
        (transfer baseDao $ calling (ep @"Transfer_ownership")
          (#newOwner :! (MkAddress $ chAddress baseDao)))
      currentAdmin <- sAdminRPC <$> getStorage @Storage (chAddress baseDao)
      assert (currentAdmin == (MkAddress $ chAddress baseDao)) "Admin address was not set"

expectNotAdmin
  :: (MonadCleveland caps m)
  => m a -> m ()
expectNotAdmin = expectFailedWith notAdmin

expectNotPendingOwner
  :: (MonadCleveland caps m)
  => m a -> m ()
expectNotPendingOwner = expectFailedWith notPendingAdmin
