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
import Test.Ligo.BaseDAO.Common (defaultQuorumThreshold)

updateOperatorsTests :: TestTree
updateOperatorsTests =
  testGroup "Update_operators:"
    [ nettestScenarioCaps "adding an operator allows transfers"
        $ addOperator
    , nettestScenarioCaps "adding an operator does not allow transfers on all tokens"
        $ addOperatorSingleToken
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
  DaoOriginateData{..} <- originateWithCustomToken defaultQuorumThreshold
  let params = FA2.OperatorParam
        { opOwner = dodOwner1
        , opOperator = operator
        , opTokenId = unfrozenTokens
        }

  withSender dodOwner1 $
    call dodDao (Call @"Update_operators") [FA2.AddOperator params]
  withSender operator $
    transfer 10 unfrozenTokens dodOwner1 dodOwner2 dodDao

addOperatorSingleToken :: MonadNettest caps base m => m ()
addOperatorSingleToken = do
  operator :: Address <- newAddress "operator"
  DaoOriginateData{..} <- originateWithCustomToken defaultQuorumThreshold
  let params = FA2.OperatorParam
        { opOwner = dodOwner1
        , opOperator = operator
        , opTokenId = unfrozenTokens1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Update_operators") [FA2.AddOperator params]
  withSender operator $
    transfer 10 unfrozenTokens dodOwner1 dodOwner2 dodDao
      & expectCustomError_ #fA2_NOT_OPERATOR dodDao

removeOperator :: MonadNettest caps base m => m ()
removeOperator = do
  DaoOriginateData{..} <- originateWithCustomToken defaultQuorumThreshold
  let params = FA2.OperatorParam
        { opOwner = dodOwner1
        , opOperator = dodOperator1
        , opTokenId = unfrozenTokens
        }

  withSender dodOwner1 $
    call dodDao (Call @"Update_operators") [FA2.RemoveOperator params]
  withSender dodOperator1 $
    transfer 10 unfrozenTokens dodOwner1 dodOwner2 dodDao
      & expectCustomError_ #fA2_NOT_OPERATOR dodDao

notOwnerUpdatesOperator :: MonadNettest caps base m => m ()
notOwnerUpdatesOperator = do
  DaoOriginateData{..} <- originateWithCustomToken defaultQuorumThreshold
  let notOwnerParams = FA2.OperatorParam
        { opOwner = dodOwner2
        , opOperator = dodOwner1
        , opTokenId = unfrozenTokens
        }

  withSender dodOwner1 $ do
    call dodDao (Call @"Update_operators") ([FA2.AddOperator notOwnerParams])
      & expectCustomErrorNoArg #nOT_OWNER dodDao
    call dodDao (Call @"Update_operators") [FA2.RemoveOperator notOwnerParams]
      & expectCustomErrorNoArg #nOT_OWNER dodDao

adminUnfrozenOperator :: MonadNettest caps base m => m ()
adminUnfrozenOperator = do
  DaoOriginateData{..} <- originateWithCustomToken defaultQuorumThreshold
  let params = FA2.OperatorParam
        { opOwner = dodOwner1
        , opOperator = dodAdmin
        , opTokenId = unfrozenTokens
        }

  withSender dodOwner1 $ do
    call dodDao (Call @"Update_operators") [FA2.AddOperator params]
    transfer 10 unfrozenTokens dodOwner1 dodOwner2 dodDao

frozenOperator :: MonadNettest caps base m => m ()
frozenOperator = do
  DaoOriginateData{..} <- originateWithCustomToken defaultQuorumThreshold
  let params op = FA2.OperatorParam
        { opOwner = dodOwner1
        , opOperator = op
        , opTokenId = frozenTokens
        }

  withSender dodOwner1 $
    call dodDao (Call @"Update_operators") [FA2.AddOperator (params dodAdmin)]
      & expectCustomErrorNoArg #oPERATION_PROHIBITED dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Update_operators") [FA2.AddOperator (params dodOwner2)]
      & expectCustomErrorNoArg #oPERATION_PROHIBITED dodDao

  withSender dodAdmin $
    call dodDao (Call @"Update_operators") [FA2.AddOperator (params dodAdmin)]
      & expectCustomErrorNoArg #oPERATION_PROHIBITED dodDao
