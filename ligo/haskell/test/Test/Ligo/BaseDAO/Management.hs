-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to the
-- `withOriginated` function
module Test.Ligo.BaseDAO.Management
  ( test_BaseDAO_Management

  , expectMigrated
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)
import Named (defaults, (!))

import Lorentz as L hiding (now, (>>))
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Tezos.Core (unsafeMkMutez)
import Util.Named

import Ligo.BaseDAO.Contract
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
  baseDao <- originateLargeUntyped $ UntypedOriginateData
    { uodName = "BaseDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal $ storageFn addresses
    , uodContract = convertContract baseDAOContractLigo
    }

  tests addresses (TAddress baseDao)

-- | We test non-token entrypoints of the BaseDAO contract here
test_BaseDAO_Management :: [TestTree]
test_BaseDAO_Management =
  [ testGroup "Ownership transfer"
    [ nettestScenarioCaps "Contract forbids XTZ transfer" $ do
        now <- getNow
        withOriginated 2 (\(owner:_) -> initialStorage now owner) $ \[owner, wallet1] baseDao ->
          withSender (AddressResolved owner) $ transfer TransferData
            { tdTo = AddressResolved $ unTAddress baseDao
            , tdAmount = unsafeMkMutez 1
            , tdEntrypoint = unsafeBuildEpName "transfer_ownership"
            , tdParameter = (#newOwner .! wallet1)
            }
          & expectForbiddenXTZ

    , nettestScenarioCaps "transfer ownership entrypoint authenticates sender" $ do
        now <- getNow
        transferOwnership withOriginated (initialStorage now)

    , nettestScenarioCaps "sets pending owner" $ do
        now <- getNow
        transferOwnership withOriginated (initialStorage now)

    , nettestScenarioCaps "does not set administrator" $ do
        now <- getNow
        notSetAdmin withOriginated (initialStorage now)

    , nettestScenarioCaps "rewrite existing pending owner" $ do
        now <- getNow
        rewritePendingOwner withOriginated (initialStorage now)

    , nettestScenarioCaps "invalidates pending owner if new owner is current admin" $ do
        now <- getNow
        invalidatePendingOwner withOriginated (initialStorage now)

    , nettestScenarioCaps "Respects migration state" $ do
        now <- getNow
        respectMigratedState withOriginated (initialStorage now)
    ]
  , testGroup "Accept Ownership"
      [ nettestScenarioCaps "authenticates the sender" $ do
          now <- getNow
          authenticateSender withOriginated (initialStorage now)

       , nettestScenarioCaps "changes the administrator to pending owner" $ do
          now <- getNow
          changeToPendingAdmin withOriginated (initialStorage now)

       , nettestScenarioCaps "throws error when there is no pending owner" $ do
          now <- getNow
          noPendingAdmin withOriginated (initialStorage now)

       , nettestScenarioCaps "throws error when called by current admin, when pending owner is not the same" $ do
          now <- getNow
          pendingOwnerNotTheSame withOriginated (initialStorage now)

      , nettestScenarioCaps "Respects migration state" $ do
          now <- getNow
          acceptOwnerRespectMigration withOriginated (initialStorage now)
      ]

  , testGroup "Migration"
      [ nettestScenarioCaps "authenticates the sender" $ do
          now <- getNow
          migrationAuthenticateSender withOriginated (initialStorage now)

      , nettestScenarioCaps "successfully sets the pending migration address " $ do
          now <- getNow
          migrationSetPendingOwner withOriginated (initialStorage now)

      , nettestScenarioCaps "overwrites previous migration target" $ do
          now <- getNow
          migrationOverwritePrevious withOriginated (initialStorage now)

      , nettestScenarioCaps "allows calls until confirm migration is called" $ do
          now <- getNow
          migrationAllowCallUntilConfirm withOriginated (initialStorage now)
      ]

  , testGroup "Confirm Migration"
      [ nettestScenarioCaps "authenticates the sender" $ do
          now <- getNow
          confirmMigAuthenticateSender withOriginated (initialStorage now)

      , nettestScenarioCaps "authenticates migration state" $ do
          now <- getNow
          confirmMigAuthenticateState withOriginated (initialStorage now)

      , nettestScenarioCaps "finalizes migration" $ do
          now <- getNow
          confirmMigFinalize withOriginated (initialStorage now)

     ]

  ]

  where
    testCustomEntrypoint :: ('[(ByteString, FullStorage)] :-> '[([Operation], StorageL)])
    testCustomEntrypoint =
      -- Unpack an address from packed bytes and set it as admin
      L.unpair #
      L.unpackRaw @Address #
      L.ifNone
        (L.unit # L.failWith)
        (L.dip (L.toField #fsStorage) # setField #sAdmin) #
      L.nil # pair

    initialStorage now admin = mkFullStorageL
      ! #admin admin
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #customEps
          [ ([mt|testCustomEp|], lPackValueRaw testCustomEntrypoint)
          ]
      ! defaults

type WithOriginateFn m param st = Integer
  -> ([Address] -> st)
  -> ([Address] -> TAddress param  -> m ())
  -> m ()

type WithStorage st = Address -> st

transferOwnership
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
transferOwnership withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
    withSender (AddressResolved wallet1) $ call baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
      & expectNotAdmin

notSetAdmin
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
notSetAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender (AddressResolved owner) $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        -- Make the call once again to make sure the admin still retains admin
        -- privileges
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)

rewritePendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
rewritePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender (AddressResolved owner) $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet2)
      -- Make the accept ownership call from wallet1 and see that it fails
      withSender (AddressResolved wallet1) $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner
      -- Make the accept ownership call from wallet1 and see that it works
      withSender (AddressResolved wallet2) $ call baseDao (Call @"Accept_ownership") ()

invalidatePendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
invalidatePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender (AddressResolved owner) $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! owner)
      -- Make the accept ownership call from wallet1 and see that it fails
      -- with 'not pending owner' error
      withSender (AddressResolved wallet1) $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner

respectMigratedState
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
respectMigratedState withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, newOwner] baseDao -> do
      withSender (AddressResolved owner) $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender (AddressResolved newAddress1) $
        call baseDao (Call @"Confirm_migration") ()
      withSender (AddressResolved owner) $
        call baseDao (Call @"Transfer_ownership")
        (#newOwner .! newOwner)
        & expectMigrated newAddress1

authenticateSender
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
authenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender (AddressResolved owner) $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender (AddressResolved wallet2) $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner

changeToPendingAdmin
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
changeToPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender (AddressResolved owner) $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender (AddressResolved wallet1) $ call baseDao (Call @"Accept_ownership") ()

noPendingAdmin
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
noPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, wallet1] baseDao -> do
      withSender (AddressResolved wallet1) $
        call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner

pendingOwnerNotTheSame
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
pendingOwnerNotTheSame withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> withSender (AddressResolved owner) $ do
      call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      call baseDao (Call @"Accept_ownership") ()
      & expectNotPendingOwner

acceptOwnerRespectMigration
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
acceptOwnerRespectMigration withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1] baseDao -> do
      withSender (AddressResolved owner) $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender (AddressResolved newAddress1) $
        call baseDao (Call @"Confirm_migration") ()
      withSender (AddressResolved owner) $ call
        baseDao (Call @"Accept_ownership") ()
        & expectMigrated newAddress1


migrationAuthenticateSender
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationAuthenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[_, newAddress1, randomAddress] baseDao -> do
      withSender (AddressResolved randomAddress) $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        & expectNotAdmin

migrationSetPendingOwner
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationSetPendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1] baseDao -> do
      withSender (AddressResolved owner) $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender (AddressResolved newAddress1) $
        call baseDao (Call @"Confirm_migration") ()

migrationOverwritePrevious
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationOverwritePrevious withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, newAddress2] baseDao -> do
      withSender(AddressResolved owner) $ do
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        call baseDao (Call @"Migrate") (#newAddress .! newAddress2)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender (AddressResolved newAddress2) $
        call baseDao (Call @"Confirm_migration") ()

migrationAllowCallUntilConfirm
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
migrationAllowCallUntilConfirm withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, newAddress2] baseDao -> withSender (AddressResolved owner) $ do
      call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      call baseDao (Call @"Migrate") (#newAddress .! newAddress2)

confirmMigAuthenticateSender
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
confirmMigAuthenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1, randomAddress] baseDao -> do
      withSender (AddressResolved owner) $ call
        baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender (AddressResolved randomAddress) $
        call baseDao (Call @"Confirm_migration") ()
        & expectNotMigrationTarget

confirmMigAuthenticateState
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
confirmMigAuthenticateState withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, newAddress1] baseDao -> do
      withSender (AddressResolved newAddress1) $
        call baseDao (Call @"Confirm_migration") ()
        & expectNotMigrating

confirmMigFinalize
  :: forall caps base m param pm st
  . (MonadNettest caps base m, ParameterC param pm)
  => WithOriginateFn m param st -> WithStorage st -> m ()
confirmMigFinalize withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, newAddress1] baseDao -> do
      withSender (AddressResolved owner) $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
      -- We test this by calling `confirmMigration` and seeing that it does not fail
      withSender (AddressResolved newAddress1) $
        call baseDao (Call @"Confirm_migration") ()
      withSender (AddressResolved owner) $
        call baseDao (Call @"Migrate") (#newAddress .! newAddress1)
        & expectMigrated newAddress1

expectNotAdmin
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotAdmin = expectCustomErrorNoArg #nOT_ADMIN

expectNotPendingOwner
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotPendingOwner = expectCustomErrorNoArg #nOT_PENDING_ADMIN

expectNotMigrating
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotMigrating = expectCustomErrorNoArg #nOT_MIGRATING

expectNotMigrationTarget
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotMigrationTarget = expectCustomErrorNoArg #nOT_MIGRATION_TARGET

expectMigrated
  :: (MonadNettest caps base m)
  => Address -> m a -> m ()
expectMigrated addr = expectCustomError #mIGRATED addr

expectForbiddenXTZ
  :: (MonadNettest caps base m)
  => m a -> m ()
expectForbiddenXTZ = expectCustomErrorNoArg #fORBIDDEN_XTZ
