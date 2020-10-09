-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func

module Test.Management
  ( test_baseDAO
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Contracts.BaseDAO (mkStorage)
import Morley.Nettest
import Test.Common
import Util.Named

-- | We test non-token entrypoints of the BaseDAO contract here
test_baseDAO :: [TestTree]
test_baseDAO =
  [ testGroup "Ownership transfer"
    [ testCase "transfer ownership entrypoint authenticates sender" $
        nettestTestExpectation $ uncapsNettest $ do
          withOriginated 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
            callFrom (AddressResolved wallet1) baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
              & expectNotAdmin

    , testCase "sets pending owner" $
        nettestTestExpectation $ uncapsNettest $ do
          withOriginated 2 (\(owner:_) -> initialStorage owner) $
            \[owner, wallet1] baseDao -> do
                callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                  (#newOwner .! wallet1)
                callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()

    , testCase "does not set administrator" $
        nettestTestExpectation $ uncapsNettest $ do
          withOriginated 2 (\(owner:_) -> initialStorage owner) $
            \[owner, wallet1] baseDao -> do
              callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                (#newOwner .! wallet1)
              -- Make the call once again to make sure the admin still retains admin
              -- privileges
              callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                (#newOwner .! wallet1)

    , testCase "rewrite existing pending owner" $
        nettestTestExpectation $ uncapsNettest $ do
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

    , testCase "invalidates pending owner if new owner is current admin" $
        nettestTestExpectation $ uncapsNettest $ do
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
    ]
  , testGroup "Accept Ownership"
      [ testCase "authenticates the sender" $
          nettestTestExpectation $ uncapsNettest $ do
            withOriginated 3 (\(owner:_) -> initialStorage owner) $
              \[owner, wallet1, wallet2] baseDao -> do
                callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                  (#newOwner .! wallet1)
                callFrom (AddressResolved wallet2) baseDao (Call @"Accept_ownership") ()
                  & expectNotPendingOwner

       , testCase "changes the administrator to pending owner and resets pending owner" $
           nettestTestExpectation $ uncapsNettest $ do
             withOriginated 2 (\(owner:_) -> initialStorage owner) $
               \[owner, wallet1] baseDao -> do
                 callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                   (#newOwner .! wallet1)
                 callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
                 -- Make a second call and see that it fails with 'no pending owner set' error
                 callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
                  & expectNoPendingOwnerSet

       , testCase "throws error when there is no pending owner" $
           nettestTestExpectation $ uncapsNettest $ do
             withOriginated 2 (\(owner:_) -> initialStorage owner) $
               \[_, wallet1] baseDao -> do
                 callFrom (AddressResolved wallet1) baseDao (Call @"Accept_ownership") ()
                  & expectNoPendingOwnerSet

       , testCase "throws error when called by current admin, when pending owner is not the same" $
           nettestTestExpectation $ uncapsNettest $ do
             withOriginated 2 (\(owner:_) -> initialStorage owner) $
               \[owner, wallet1] baseDao -> do
                 callFrom (AddressResolved owner) baseDao (Call @"Transfer_ownership")
                   (#newOwner .! wallet1)
                 callFrom (AddressResolved owner) baseDao (Call @"Accept_ownership") ()
                  & expectNotPendingOwner
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
