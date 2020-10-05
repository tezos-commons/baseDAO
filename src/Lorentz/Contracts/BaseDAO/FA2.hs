-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.FA2
   ( transfer
   , balanceOf
   , tokenMetadataRegistry
   , permissionsDescriptor
   , updateOperators
   ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface hiding (name, sender)

-- | TODO: Implement missing methods
transfer :: Entrypoint TransferParams Storage
transfer = doNothing

balanceOf :: Entrypoint BalanceRequestParams Storage
balanceOf = doNothing

permissionsDescriptor :: Entrypoint PermissionsDescriptorParam Storage
permissionsDescriptor = doNothing

doNothing :: Entrypoint a b
doNothing = do
  drop
  nil
  pair

tokenMetadataRegistry :: Entrypoint TokenMetadataRegistryParam Storage
tokenMetadataRegistry = do
  swap
  getField #sTokenAddress
  dig @2
  swap
  push @Mutez (toMutez 0)
  swap
  transferTokens # nil # swap # cons # pair

updateOperators :: Entrypoint UpdateOperatorsParam Storage
updateOperators = do
  iter $
    caseT @UpdateOperator
      ( #cAdd_operator /-> addOperator
      , #cRemove_operator /-> removeOperator
      )
  nil; pair

addOperator :: forall s. OperatorParam & Storage & s :-> Storage & s
addOperator = do
  unpair
  fromNamed #owner
  swap
  unpair
  fromNamed #operator
  swap
  fromNamed #token_id
  dup
  assertNeq0or1
  dug @2
  swap
  dupTop2
  assertNeq [mt|NOT_OWNER|]
  dug @2
  pair
  swap
  pair
  swap
  getField #sOperators
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: (Address, (Address, TokenId)) & Operators & Storage & s :-> Storage & s
    ifKeyExists = dropN @2

    ifKeyDoesntExist :: (Address, (Address, TokenId)) & Operators & Storage & s :-> Storage & s
    ifKeyDoesntExist = do
      unit
      some
      swap
      update
      setField #sOperators

removeOperator :: forall s. OperatorParam & Storage & s :-> Storage & s
removeOperator = do
  unpair
  fromNamed #owner
  swap
  unpair
  fromNamed #operator
  swap
  fromNamed #token_id
  dup
  assertNeq0or1
  dug @2
  swap
  dupTop2
  assertNeq [mt|NOT_OWNER|]
  dug @2
  pair
  swap
  pair
  swap
  getField #sOperators
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: (Address, (Address, TokenId)) & Operators & Storage & s :-> Storage & s
    ifKeyExists = do
      push Nothing
      swap
      update
      setField #sOperators


    ifKeyDoesntExist :: (Address, (Address, TokenId)) & Operators & Storage & s :-> Storage & s
    ifKeyDoesntExist = dropN @2

-- | TODO: Move this one to Lorentz
dupTop2 :: a & b & f :-> a & b & a & b & f
dupTop2 = do
  duupX @2
  duupX @2

-- | TODO: Probably this one can be moved to Lorentz as well
assertNeq0or1 :: Natural & f :-> f
assertNeq0or1 = do
  dup
  push @Natural 0
  if IsEq
  then drop @Natural
  else do
    dup
    push @Natural 1
    if IsEq
    then failUsing [mt|FA2_OPERATION_PROHIBITED|]
    else failUsing [mt|FA2_TOKEN_UNDEFINED|]
