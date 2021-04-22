-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to the
-- `withOriginated` function
module Test.Ligo.BaseDAO.Management
  ( test_BaseDAO_Management
  ) where

import Universum

import Named (defaults, (!))
import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (now, (>>))
import Michelson.Runtime.GState (genesisAddress)
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
  -> ([Address] -> TAddress Parameter -> m a)
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
          withSender owner $ transfer TransferData
            { tdTo = unTAddress baseDao
            , tdAmount = unsafeMkMutez 1
            , tdEntrypoint = unsafeBuildEpName "transfer_ownership"
            , tdParameter = (#newOwner .! wallet1)
            }
          & expectForbiddenXTZ baseDao

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
      ]
  ]

  where
    testCustomEntrypoint :: ('[(ByteString, FullStorage)] :-> '[([Operation], Storage)])
    testCustomEntrypoint =
      -- Unpack an address from packed bytes and set it as admin
      L.unpair #
      L.unpackRaw @Address #
      L.ifNone
        (L.unit # L.failWith)
        (L.dip (L.toField #fsStorage) # setField #sAdmin) #
      L.nil # pair

    initialStorage now admin = mkFullStorage
      ! #admin admin
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress genesisAddress
      ! #customEps
          [ ([mt|testCustomEp|], lPackValueRaw testCustomEntrypoint)
          ]
      ! defaults

type WithOriginateFn m = Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> TAddress Parameter  -> m ())
  -> m ()

type WithStorage = Address -> FullStorage

transferOwnership
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
transferOwnership withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
    withSender wallet1 $ call baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
      & expectNotAdmin baseDao

authenticateSender
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
authenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender wallet2 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner baseDao

changeToPendingAdmin
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
changeToPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()

noPendingAdmin
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
noPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, wallet1] baseDao -> do
      withSender wallet1 $
        call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner baseDao

pendingOwnerNotTheSame
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
pendingOwnerNotTheSame withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> withSender owner $ do
      call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      call baseDao (Call @"Accept_ownership") ()
      & expectNotPendingOwner baseDao

notSetAdmin
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
notSetAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
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
  => WithOriginateFn m -> WithStorage -> m ()
rewritePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
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
  => WithOriginateFn m -> WithStorage -> m ()
invalidatePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
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

expectNotAdmin
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectNotAdmin = expectCustomErrorNoArg #nOT_ADMIN

expectNotPendingOwner
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectNotPendingOwner = expectCustomErrorNoArg #nOT_PENDING_ADMIN

expectForbiddenXTZ
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectForbiddenXTZ = expectCustomErrorNoArg #fORBIDDEN_XTZ
