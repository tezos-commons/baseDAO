-- SPDX-FileCopyrightText: 2020 TQ Tezos
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

import Lorentz as L
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Tezos.Core (unsafeMkMutez)
import Util.Named

import BaseDAO.ShareTest.Management
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
