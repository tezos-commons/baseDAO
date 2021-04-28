-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token.UpdateOperators
  ( updateOperatorsTests
  ) where

import Universum

import Lorentz (Address)
import Morley.Nettest hiding (transfer)
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.Tasty (TestTree, testGroup)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Token.Common

updateOperatorsTests :: TestTree
updateOperatorsTests =
  testGroup "Update_operators:"
    [ nettestScenarioCaps "adding an operator allows transfers"
        $ addOperator
    , nettestScenarioCaps "removing an operator prohibits transfers"
        $ removeOperator
    , nettestScenarioCaps "cannot update someone else's operators"
        $ notOwnerUpdatesOperator
    , nettestScenarioCaps "admin can become an operator of unfrozen tokens"
        $ adminUnfrozenOperator
    , nettestScenarioCaps "no-one can become an operator of frozen tokens"
        $ frozenOperator
    ]

addOperator :: MonadNettest caps base m => m ()
addOperator = do
  operator :: Address <- newAddress "operator"
  ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = operator
        , opTokenId = unfrozenTokens
        }

  withSender owner1 $
    call dao (Call @"Update_operators") [FA2.AddOperator params]
  withSender operator $
    transfer 10 unfrozenTokens owner1 owner2 dao

removeOperator :: MonadNettest caps base m => m ()
removeOperator = do
  ((owner1, op1), (owner2, _), dao, _, _) <- originateWithCustomToken
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = op1
        , opTokenId = unfrozenTokens
        }

  withSender owner1 $
    call dao (Call @"Update_operators") [FA2.RemoveOperator params]
  withSender op1 $
    transfer 10 unfrozenTokens owner1 owner2 dao
      & expectCustomError_ #fA2_NOT_OPERATOR dao

notOwnerUpdatesOperator :: MonadNettest caps base m => m ()
notOwnerUpdatesOperator = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateWithCustomToken
  let notOwnerParams = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = owner1
        , opTokenId = unfrozenTokens
        }

  withSender owner1 $ do
    call dao (Call @"Update_operators") ([FA2.AddOperator notOwnerParams])
      & expectCustomErrorNoArg #nOT_OWNER dao
    call dao (Call @"Update_operators") [FA2.RemoveOperator notOwnerParams]
      & expectCustomErrorNoArg #nOT_OWNER dao

adminUnfrozenOperator :: MonadNettest caps base m => m ()
adminUnfrozenOperator = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken
  let params = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = admin
        , opTokenId = unfrozenTokens
        }

  withSender owner1 $ do
    call dao (Call @"Update_operators") [FA2.AddOperator params]
    transfer 10 unfrozenTokens owner1 owner2 dao

frozenOperator :: MonadNettest caps base m => m ()
frozenOperator = do
  ((owner1, _), (owner2, _), dao, _, admin) <- originateWithCustomToken
  let params op = FA2.OperatorParam
        { opOwner = owner1
        , opOperator = op
        , opTokenId = frozenTokens
        }

  withSender owner1 $
    call dao (Call @"Update_operators") [FA2.AddOperator (params admin)]
      & expectCustomErrorNoArg #oPERATION_PROHIBITED dao

  withSender owner1 $
    call dao (Call @"Update_operators") [FA2.AddOperator (params owner2)]
      & expectCustomErrorNoArg #oPERATION_PROHIBITED dao

  withSender admin $
    call dao (Call @"Update_operators") [FA2.AddOperator (params admin)]
      & expectCustomErrorNoArg #oPERATION_PROHIBITED dao
