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

import Test.Tasty (TestTree, testGroup)
import Named (defaults, (!))

import Lorentz as L hiding (now)
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Tezos.Core (unsafeMkMutez)
import Util.Named

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.ShareTest.Management
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
