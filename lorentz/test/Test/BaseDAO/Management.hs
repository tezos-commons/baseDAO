-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.BaseDAO.Management
  ( test_BaseDAO_Management

  , expectMigrated
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Named (defaults, (!))
import Test.Tasty (TestTree, testGroup)
import Util.Named

import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types

-- | Function that originates the contract and also makes a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> TAddress (Parameter () Empty) -> m a)
  -> m a
withOriginated addrCount tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  let (owner:_) = addresses
  baseDao <- originateLarge $ OriginateData
    { odName = "BaseDAO Test Contract"
    , odBalance = zeroMutez
    , odStorage = initialStorage owner
    , odContract = DAO.baseDaoContract DAO.defaultConfig
    }
  tests addresses baseDao

initialStorage :: Address -> DAO.Storage () pm
initialStorage admin =
  mkStorage ! #admin admin ! #extra () ! #metadata mempty ! defaults

-- | We test non-token entrypoints of the BaseDAO contract here
test_BaseDAO_Management :: [TestTree]
test_BaseDAO_Management =
  -- TODO: [#91] Disabled for now due to unable to send XTZ
  -- [ nettestScenarioCaps "Contract forbids XTZ transfer" $
  --     withOriginated 2 $ \[_, wallet1] baseDao ->
  --       transfer TransferData
  --         { tdFrom = AddressResolved wallet1
  --         , tdTo = AddressResolved $ unTAddress baseDao
  --         , tdAmount = unsafeMkMutez 1
  --         , tdEntrypoint = unsafeBuildEpName "transfer_ownership"
  --         , tdParameter = (#newOwner .! wallet1)
  --         }
  --       & expectCustomErrorNoArg #fORBIDDEN_XTZ baseDao

  [ testGroup "Ownership transfer"
    [ nettestScenarioCaps "transfer ownership entrypoint authenticates sender" $
        transferOwnership

    , nettestScenarioCaps "sets pending owner" $
        transferOwnership

    , nettestScenarioCaps "does not set administrator" $
        notSetAdmin

    , nettestScenarioCaps "rewrite existing pending owner" $
        rewritePendingOwner

    , nettestScenarioCaps "invalidates pending owner if new owner is current admin" $
        invalidatePendingOwner

    , nettestScenarioCaps "Respects migration state" $
        respectMigratedState
    ]
  , testGroup "Accept Ownership"
      [ nettestScenarioCaps "authenticates the sender" $
          authenticateSender

       , nettestScenarioCaps "changes the administrator to pending owner" $
          changeToPendingAdmin

       , nettestScenarioCaps "throws error when there is no pending owner" $
          noPendingAdmin

       , nettestScenarioCaps "throws error when called by current admin, when pending owner is not the same" $
          pendingOwnerNotTheSame

      , nettestScenarioCaps "Respects migration state" $
          acceptOwnerRespectMigration
      ]

  , testGroup "Migration"
      [ nettestScenarioCaps "authenticates the sender" $
          migrationAuthenticateSender

      , nettestScenarioCaps "successfully sets the pending migration address " $
          migrationSetTarget

      , nettestScenarioCaps "overwrites previous migration target" $
          migrationOverwritePrevious

      , nettestScenarioCaps "allows calls until confirm migration is called" $
          migrationAllowCallUntilConfirm
      ]

  , testGroup "Confirm Migration"
      [ nettestScenarioCaps "authenticates the sender" $
          confirmMigAuthenticateSender

      , nettestScenarioCaps "authenticates migration state" $
          confirmMigAuthenticateState

      , nettestScenarioCaps "finalizes migration" $
          confirmMigFinalize

     ]
  ]


transferOwnership
  :: MonadNettest caps base m
  => m ()
transferOwnership = withOriginated 2 $ \[_, wallet1] baseDao ->
    withSender wallet1 $ call baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
      & expectNotAdmin baseDao

notSetAdmin
  :: MonadNettest caps base m
  => m ()
notSetAdmin = withOriginated 2 $
    \[owner, wallet1] baseDao -> do
      withSender owner $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        -- Make the call once again to make sure the admin still retains admin
        -- privileges
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)

rewritePendingOwner
  :: MonadNettest caps base m
  => m ()
rewritePendingOwner = withOriginated 3 $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet2)
      -- Make the accept ownership call from wallet1 and see that it fails
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner baseDao
      -- Make the accept ownership call from wallet1 and see that it works
      withSender wallet2 $ call baseDao (Call @"Accept_ownership") ()

invalidatePendingOwner
  :: MonadNettest caps base m
  => m ()
invalidatePendingOwner = withOriginated 2 $
    \[owner, wallet1] baseDao -> do
      withSender owner $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! owner)
      -- Make the accept ownership call from wallet1 and see that it fails
      -- with 'not pending owner' error
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner baseDao

respectMigratedState
  :: MonadNettest caps base m
  => m ()
respectMigratedState = withOriginated 3 $
    \[owner, newAddress1, newOwner] baseDao -> do
      withSender owner $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender newAddress1 $
        call baseDao (Call @"Confirm_migration") ()
      withSender owner $
        call baseDao (Call @"Transfer_ownership")
        (#newOwner .! newOwner)
        & expectMigrated baseDao newAddress1

authenticateSender
  :: MonadNettest caps base m
  => m ()
authenticateSender = withOriginated 3 $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender wallet2 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner baseDao
changeToPendingAdmin
  :: MonadNettest caps base m
  => m ()
changeToPendingAdmin = withOriginated 2 $
    \[owner, wallet1] baseDao -> do
      withSender owner $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()

noPendingAdmin
  :: MonadNettest caps base m
  => m ()
noPendingAdmin = withOriginated 2 $
    \[_, wallet1] baseDao -> do
      withSender wallet1 $
        call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner baseDao

pendingOwnerNotTheSame
  :: MonadNettest caps base m
  => m ()
pendingOwnerNotTheSame = withOriginated 2 $
    \[owner, wallet1] baseDao -> withSender owner $ do
      call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      call baseDao (Call @"Accept_ownership") ()
      & expectNotPendingOwner baseDao

acceptOwnerRespectMigration
  :: MonadNettest caps base m
  => m ()
acceptOwnerRespectMigration = withOriginated 2 $
    \[owner, newAddress1] baseDao -> do
      withSender owner $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender newAddress1 $
        call baseDao (Call @"Confirm_migration") ()
      withSender owner $ call
        baseDao (Call @"Accept_ownership") ()
        & expectMigrated baseDao newAddress1


migrationAuthenticateSender
  :: MonadNettest caps base m
  => m ()
migrationAuthenticateSender = withOriginated 3 $
    \[_, newAddress1, randomAddress] baseDao -> do
      withSender randomAddress $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        & expectNotAdmin baseDao

migrationSetTarget
  :: MonadNettest caps base m
  => m ()
migrationSetTarget = withOriginated 2 $
    \[owner, newAddress1] baseDao -> do
      withSender owner $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender newAddress1 $
        call baseDao (Call @"Confirm_migration") ()

migrationOverwritePrevious
  :: MonadNettest caps base m
  => m ()
migrationOverwritePrevious = withOriginated 3 $
    \[owner, newAddress1, newAddress2] baseDao -> do
      withSender owner $ do
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        call baseDao (Call @"Migrate") (#newAddress .! newAddress2)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender newAddress2 $
        call baseDao (Call @"Confirm_migration") ()

migrationAllowCallUntilConfirm
  :: MonadNettest caps base m
  => m ()
migrationAllowCallUntilConfirm = withOriginated 3 $
    \[owner, newAddress1, newAddress2] baseDao -> withSender owner $ do
      call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      call baseDao (Call @"Migrate") (#newAddress .! newAddress2)

confirmMigAuthenticateSender
  :: MonadNettest caps base m
  => m ()
confirmMigAuthenticateSender = withOriginated 3 $
    \[owner, newAddress1, randomAddress] baseDao -> do
      withSender owner $ call
        baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender randomAddress $
        call baseDao (Call @"Confirm_migration") ()
        & expectNotMigrationTarget baseDao

confirmMigAuthenticateState
  :: MonadNettest caps base m
  => m ()
confirmMigAuthenticateState = withOriginated 2 $
    \[_, newAddress1] baseDao -> do
      withSender newAddress1 $
        call baseDao (Call @"Confirm_migration") ()
        & expectNotMigrating baseDao

confirmMigFinalize
  :: MonadNettest caps base m
  => m ()
confirmMigFinalize = withOriginated 2 $
    \[owner, newAddress1] baseDao -> do
      withSender owner $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender newAddress1 $
        call baseDao (Call @"Confirm_migration") ()
      withSender owner $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        & expectMigrated baseDao newAddress1

expectNotAdmin
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectNotAdmin = expectCustomErrorNoArg #nOT_ADMIN

expectNotPendingOwner
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectNotPendingOwner = expectCustomErrorNoArg #nOT_PENDING_ADMIN

expectNotMigrating
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectNotMigrating = expectCustomErrorNoArg #nOT_MIGRATING

expectNotMigrationTarget
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectNotMigrationTarget = expectCustomErrorNoArg #nOT_MIGRATION_TARGET

expectMigrated
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> Address -> m a -> m ()
expectMigrated addr = expectCustomError #mIGRATED addr
