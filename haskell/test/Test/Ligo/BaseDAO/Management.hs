-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.BaseDAO.Management
  ( test_BaseDAO_Management
  ) where

import Universum

import Named (defaults)
import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (now, (>>))
import Morley.Michelson.Runtime.GState (genesisAddress)
import Test.Cleveland
import Morley.Util.Peano
import Morley.Util.SizedList (SizedList)

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Management.TransferOwnership
import Test.Ligo.Common (refillables)

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: (MonadCleveland caps m, IsoNatPeano num num', SingIPeano num)
  => (SizedList num ImplicitAddress -> Storage)
  -> (SizedList num ImplicitAddress -> ContractHandle Parameter Storage () -> m a)
  -> m a
withOriginated storageFn tests = do
  addresses <- refillables $ newAddresses (enumAliases "address")
  baseDao <- originate "BaseDAO Test Contract" (storageFn addresses) baseDAOContractLigo
  tests addresses baseDao

-- | We test non-token entrypoints of the BaseDAO contract here
test_BaseDAO_Management :: [TestTree]
test_BaseDAO_Management =
  [ testGroup "Ownership transfer"
    [ testScenario "transfer ownership entrypoint authenticates sender" $ scenario $ do
        current_level <- getLevel
        transferOwnership withOriginated (initialStorage current_level)

    , testScenario "sets pending owner" $ scenario $ do
        current_level <- getLevel
        transferOwnershipSetsPendingOwner withOriginated (initialStorage current_level)

    , testScenario "does not set administrator" $ scenario $ do
        current_level <- getLevel
        notSetAdmin withOriginated (initialStorage current_level)

    , testScenario "rewrite existing pending owner" $ scenario $ do
        current_level <- getLevel
        rewritePendingOwner withOriginated (initialStorage current_level)

    , testScenario "invalidates pending owner if new owner is current admin" $ scenario $ do
        current_level <- getLevel
        invalidatePendingOwner withOriginated (initialStorage current_level)

    , testScenario "bypasses accept_entrypoint if new admin address is self" $ scenario $ do
        current_level <- getLevel
        bypassAcceptForSelf withOriginated (initialStorage current_level)
    ]
    , testGroup "Accept Ownership"
      [ testScenario "authenticates the sender" $ scenario $ do
          current_level <- getLevel
          authenticateSender withOriginated (initialStorage current_level)

      , testScenario "changes the administrator to pending owner" $ scenario $ do
          current_level <- getLevel
          changeToPendingAdmin withOriginated (initialStorage current_level)

      , testScenario "throws error when there is no pending owner" $ scenario $ do
          current_level <- getLevel
          noPendingAdmin withOriginated (initialStorage current_level)

      , testScenario "throws error when called by current admin, when pending owner is not the same" $ scenario $ do
          current_level <- getLevel
          pendingOwnerNotTheSame withOriginated (initialStorage current_level)
      ]
  ]

  where

    initialStorage currentLevel admin = mkStorage' @'Base
      ! #admin admin
      ! #extra ()
      ! #metadata mempty
      ! #level currentLevel
      ! #tokenAddress (toAddress genesisAddress)
      ! defaults
