-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.BaseDAO.Management
  ( test_BaseDAO_Management
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)
import Named (defaults, (!))

import Lorentz as L
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Tezos.Core (unsafeMkMutez)

import Ligo.BaseDAO.Contract
import Util.Named
import Ligo.BaseDAO.Types

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> TAddress ParameterL -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  baseDao <- originateUntyped $ UntypedOriginateData
    { uodFrom = nettestAddress
    , uodName = "BaseDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal $ storageFn addresses
    , uodContract = convertContract baseDAOContractLigo
    }
  tests addresses (TAddress baseDao)

-- | We test non-token entrypoints of the BaseDAO contract here
test_BaseDAO_Management :: [TestTree]
test_BaseDAO_Management =
  [ testGroup "Ownership transfer"
    [ nettestScenarioCaps "Contract forbids XTZ transfer" $
        withOriginated 2 (\(owner:_) -> initialStorage owner) $ \[owner, wallet1] baseDao ->
          transfer TransferData
            { tdFrom = AddressResolved owner
            , tdTo = AddressResolved $ unTAddress baseDao
            , tdAmount = unsafeMkMutez 1
            , tdEntrypoint = unsafeBuildEpName "transfer_ownership"
            , tdParameter = (#newOwner .! wallet1)
            }
          & expectForbiddenXTZ

    , nettestScenarioCaps "transfer ownership entrypoint authenticates sender" $
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
            -- with 'not pending owner' error
            callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
              & expectNotPendingOwner

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

       , nettestScenarioCaps "changes the administrator to pending owner" $
           withOriginated 2 (\(owner:_) -> initialStorage owner) $
             \[owner, wallet1] baseDao -> do
               callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                 (#newOwner .! wallet1)
               callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()

       , nettestScenarioCaps "throws error when there is no pending owner" $
           withOriginated 2 (\(owner:_) -> initialStorage owner) $
             \[_, wallet1] baseDao -> do
               callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
                & expectNotPendingOwner

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

  , testGroup "Custom entrypoints"
      [ nettestScenarioCaps "can call custom entrypoint" $
          withOriginated 3 (\(owner:_) -> initialStorage owner) $
            \[owner, randomAddress, newContractAddress] baseDao -> do
              -- Using a custom entry point we replace the admin address
              -- with `randomAddress`
              callFrom (AddressResolved randomAddress) baseDao (Call @"CallCustom") ([mt|testCustomEp|], lPackValueRaw randomAddress)
              -- Then we try to call an admin only endpoint as `owner`, expecting it to fail.
              callFrom (AddressResolved owner) baseDao (Call @"Migrate") (#newAddress .! newContractAddress)
                & expectNotAdmin
              -- Then we try to call an admin only endpoint as `randomAddress`, expecting it to work.
              callFrom (AddressResolved randomAddress) baseDao (Call @"Migrate") (#newAddress .! newContractAddress)
     ]
  ]
  where

    testCustomEntrypoint :: ('[(ByteString, FullStorage)] :-> '[([Operation], FullStorage)])
    testCustomEntrypoint =
      -- Unpack an address from packed bytes and set it as admin
      L.unpair #
      L.unpackRaw @Address #
      L.ifNone
        (L.unit # L.failWith)
        ((L.dip (L.getField #fsStorage)) # setField #sAdmin # setField #fsStorage) #
      L.nil # pair

    initialStorage admin = mkFullStorageL
      ! #admin admin
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #customEps
          [ ([mt|testCustomEp|], lPackValueRaw testCustomEntrypoint)
          ]
      ! defaults

expectNotAdmin
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotAdmin = expectCustomError_ #nOT_ADMIN

expectNotPendingOwner
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotPendingOwner = expectCustomError_ #nOT_PENDING_ADMIN

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
