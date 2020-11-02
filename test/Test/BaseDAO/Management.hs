-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func

module Test.BaseDAO.Management
  ( expectMigrated
  , test_BaseDAO_Management
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)

import Lorentz
import Lorentz.Contracts.BaseDAO (mkStorage)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.Common
import Tezos.Core (unsafeMkMutez)
import Util.Named

-- | We test non-token entrypoints of the BaseDAO contract here
test_BaseDAO_Management :: [TestTree]
test_BaseDAO_Management =
  [ nettestScenarioCaps "Contract forbids XTZ transfer" $
      withOriginated 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
        transfer TransferData
          { tdFrom = AddressResolved wallet1
          , tdTo = AddressResolved $ unTAddress baseDao
          , tdAmount = unsafeMkMutez 1
          , tdEntrypoint = unsafeBuildEpName "transfer_ownership"
          , tdParameter = (#newOwner .! wallet1)
          }
        & expectForbiddenXTZ

  , testGroup "Ownership transfer"
    [ nettestScenarioCaps "transfer ownership entrypoint authenticates sender" $
        withOriginated 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
          callFrom (AddressResolved wallet1) baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
            & expectNotAdmin

    , nettestScenarioCaps "sets pending owner" $
        withOriginated 2 (\(owner:_) -> initialStorage owner) $
          \[owner, wallet1] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                (#newOwner .! wallet1)
              callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()

    , nettestScenarioCaps "does not set administrator" $
        withOriginated 2 (\(owner:_) -> initialStorage owner) $
          \[owner, wallet1] baseDao -> do
            callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
              (#newOwner .! wallet1)
            -- Make the call once again to make sure the admin still retains admin
            -- privileges
            callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
              (#newOwner .! wallet1)

    , nettestScenarioCaps "rewrite existing pending owner" $
        withOriginated 3 (\(owner:_) -> initialStorage owner) $
          \[owner, wallet1, wallet2] baseDao -> do
            callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
              (#newOwner .! wallet1)
            callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
              (#newOwner .! wallet2)
            -- Make the accept ownership call from wallet1 and see that it fails
            callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
              & expectNotPendingOwner
            -- Make the accept ownership call from wallet1 and see that it works
            callFrom (AddressResolved wallet2) baseDao (Call @"Accept_ownership") ()

    , nettestScenarioCaps "invalidates pending owner if new owner is current admin" $
        withOriginated 2 (\(owner:_) -> initialStorage owner) $
          \[owner, wallet1] baseDao -> do
            callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
              (#newOwner .! wallet1)
            callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
              (#newOwner .! owner)
            -- Make the accept ownership call from wallet1 and see that it fails
            -- with 'no pending owner set' error
            callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
              & expectNoPendingOwnerSet

      , nettestScenarioCaps "Respects migration state" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1, newOwner] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              -- We test this by calling `confirmMigration` and seeing that it does not fail
              callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
              callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                (#newOwner .! newOwner)
                & expectMigrated newAddress1
    ]
  , testGroup "Accept Ownership"
      [ nettestScenarioCaps "authenticates the sender" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[owner, wallet1, wallet2] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                (#newOwner .! wallet1)
              callFrom (AddressResolved wallet2) baseDao (Call @"Accept_ownership") ()
                & expectNotPendingOwner

       , nettestScenarioCaps "changes the administrator to pending owner and resets pending owner" $
           withOriginated 2 (\(owner:_) -> initialStorage owner) $
             \[owner, wallet1] baseDao -> do
               callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                 (#newOwner .! wallet1)
               callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
               -- Make a second call and see that it fails with 'no pending owner set' error
               callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
                & expectNoPendingOwnerSet

       , nettestScenarioCaps "throws error when there is no pending owner" $
           withOriginated 2 (\(owner:_) -> initialStorage owner) $
             \[_, wallet1] baseDao -> do
               callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
                & expectNoPendingOwnerSet

       , nettestScenarioCaps "throws error when called by current admin, when pending owner is not the same" $
           withOriginated 2 (\(owner:_) -> initialStorage owner) $
             \[owner, wallet1] baseDao -> do
               callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                 (#newOwner .! wallet1)
               callFrom (AddressResolved owner) baseDao (Call @"Accept_ownership") ()
                & expectNotPendingOwner

      , nettestScenarioCaps "Respects migration state" $
          withOriginated 2 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              -- We test this by calling `confirmMigration` and seeing that it does not fail
              callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
              callFrom (AddressResolved owner) baseDao (Call @"Accept_ownership") ()
                & expectMigrated newAddress1
      ]

  , testGroup "Migration"
      [ nettestScenarioCaps "authenticates the sender" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[_, newAddress1, randomAddress] baseDao -> do
              callFrom (AddressResolved randomAddress) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
                & expectNotAdmin

      , nettestScenarioCaps "successfully sets the pending migration address " $
          withOriginated 2 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              -- We test this by calling `confirmMigration` and seeing that it does not fail
              callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()

      , nettestScenarioCaps "overwrites previous migration target" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1, newAddress2] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress2)
              -- We test this by calling `confirmMigration` and seeing that it does not fail
              callFrom (AddressResolved newAddress2) baseDao (Call @"Confirm_migration") ()

      , nettestScenarioCaps "allows calls until confirm migration is called" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1, newAddress2] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress2)
      ]

  , testGroup "Confirm Migration"
      [ nettestScenarioCaps "authenticates the sender" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1, randomAddress] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              -- We test this by calling `confirmMigration` and seeing that it does not fail
              callFrom (AddressResolved randomAddress) baseDao (Call @"Confirm_migration") ()
                & expectNotMigrationTarget

      , nettestScenarioCaps "authenticates migration state" $
          withOriginated 2 (\(owner:_) -> initialStorage owner) $
            \[_, newAddress1] baseDao -> do
              callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
                & expectNotMigrating

      , nettestScenarioCaps "finalizes migration" $
          withOriginated 2 (\(owner:_) -> initialStorage owner) $
            \[owner, newAddress1] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
              -- We test this by calling `confirmMigration` and seeing that it does not fail
              callFrom (AddressResolved newAddress1) baseDao (Call @"Confirm_migration") ()
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newAddress1)
                & expectMigrated newAddress1
     ]
  ]
  where
    initialStorage admin = mkStorage admin mempty mempty

expectNotAdmin
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotAdmin = expectCustomError_ #nOT_ADMINISTRATOR

expectNotPendingOwner
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotPendingOwner = expectCustomError_ #nOT_PENDING_ADMINISTRATOR

expectNoPendingOwnerSet
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNoPendingOwnerSet = expectCustomError_ #nO_PENDING_ADMINISTRATOR_SET

expectNotMigrating
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotMigrating = expectCustomError_ #nOT_MIGRATING

expectNotMigrationTarget
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotMigrationTarget = expectCustomError_ #nOT_MIGRATION_TARGET

expectMigrated
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectMigrated addr = expectCustomError #mIGRATED addr

expectForbiddenXTZ
  :: (MonadNettest caps base m)
  => m a -> m ()
expectForbiddenXTZ = expectCustomError_ #fORBIDDEN_XTZ
