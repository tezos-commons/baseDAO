-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.BaseDAO.Management
  ( test_BaseDAO_Management
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)
import Lorentz
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Named (defaults, (!))

import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types
import BaseDAO.ShareTest.Management

-- | Function that originates the contract and also makes a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> (Storage () ()))
  -> ([Address] -> TAddress (Parameter () Empty) -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  baseDao <- originateLarge $ OriginateData
    { odName = "BaseDAO Test Contract"
    , odBalance = zeroMutez
    , odStorage = storageFn addresses
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
  --     withOriginated 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
  --       transfer TransferData
  --         { tdFrom = AddressResolved wallet1
  --         , tdTo = AddressResolved $ unTAddress baseDao
  --         , tdAmount = unsafeMkMutez 1
  --         , tdEntrypoint = unsafeBuildEpName "transfer_ownership"
  --         , tdParameter = (#newOwner .! wallet1)
  --         }
  --       & expectForbiddenXTZ

  [ testGroup "Ownership transfer"
    [ nettestScenarioCaps "transfer ownership entrypoint authenticates sender" $
        transferOwnership withOriginated initialStorage

    , nettestScenarioCaps "sets pending owner" $
        transferOwnership withOriginated initialStorage

    , nettestScenarioCaps "does not set administrator" $
        notSetAdmin withOriginated initialStorage

    , nettestScenarioCaps "rewrite existing pending owner" $
        rewritePendingOwner withOriginated initialStorage

    , nettestScenarioCaps "invalidates pending owner if new owner is current admin" $
        invalidatePendingOwner withOriginated initialStorage

    , nettestScenarioCaps "Respects migration state" $
        respectMigratedState withOriginated initialStorage
    ]
  , testGroup "Accept Ownership"
      [ nettestScenarioCaps "authenticates the sender" $
          authenticateSender withOriginated initialStorage

       , nettestScenarioCaps "changes the administrator to pending owner" $
          changeToPendingAdmin withOriginated initialStorage

       , nettestScenarioCaps "throws error when there is no pending owner" $
          noPendingAdmin withOriginated initialStorage

       , nettestScenarioCaps "throws error when called by current admin, when pending owner is not the same" $
          pendingOwnerNotTheSame withOriginated initialStorage

      , nettestScenarioCaps "Respects migration state" $
          acceptOwnerRespectMigration withOriginated initialStorage
      ]

  , testGroup "Migration"
      [ nettestScenarioCaps "authenticates the sender" $
          migrationAuthenticateSender withOriginated initialStorage

      , nettestScenarioCaps "successfully sets the pending migration address " $
          migrationSetPendingOwner withOriginated initialStorage

      , nettestScenarioCaps "overwrites previous migration target" $
          migrationOverwritePrevious withOriginated initialStorage

      , nettestScenarioCaps "allows calls until confirm migration is called" $
          migrationAllowCallUntilConfirm withOriginated initialStorage
      ]

  , testGroup "Confirm Migration"
      [ nettestScenarioCaps "authenticates the sender" $
          confirmMigAuthenticateSender withOriginated initialStorage

      , nettestScenarioCaps "authenticates migration state" $
          confirmMigAuthenticateState withOriginated initialStorage

      , nettestScenarioCaps "finalizes migration" $
          confirmMigFinalize withOriginated initialStorage

     ]
  ]
