-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.FA2
   ( transfer
   , balanceOf
   , tokenMetadataRegistry
   , permissionsDescriptor
   , updateOperators
   , defaultPermissionsDescriptor
   ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface

defaultPermissionsDescriptor :: PermissionsDescriptor
defaultPermissionsDescriptor = PermissionsDescriptor
  { pdOperator = OwnerOrOperatorTransfer
  , pdReceiver = OwnerNoHook
  , pdSender = OwnerNoHook
  , pdCustom = Nothing
  }

-- | TODO: Implement missing methods
transfer :: PermissionsDescriptor -> Entrypoint TransferParams Storage
transfer pdsec = do
  iter transferItem
  nil; pair
  where
    transferItem = do
      getField #tiFrom
      swap
      getField #tiTxs
      swap
      drop @TransferItem
      iter transferOne
      drop @Address

    transferOne :: forall s. TransferDestination & Address & Storage & s :-> Address & Storage & s
    transferOne = do
      getField #tdTokenId
      swap
      dug @3
      swap
      checkAdmin (pdOperator pdsec)
      swap
      drop @TokenId
      dig @2
      getField #tdTo
      swap
      getField #tdAmount
      swap
      getField #tdTokenId
      swap
      drop @TransferDestination
      dup
      push @Natural 0
      assertEq [mt|#FA2_TOKEN_UNDEFINED|]
      swap
      swap
      pair
      dup
      dig @3
      dup
      dug @5
      dig @3
      dug @2
      dig @4
      debitFrom;
      creditTo;
      swap

    checkAdmin
      :: forall f. OwnerOrOperatorTransfer
      ->  Address & TokenId & Storage & f
      :-> Address & TokenId & Storage & f
    checkAdmin NoTransfer = failUsing [mt|#fA2_TX_DENIED|]
    checkAdmin OwnerTransfer = do
      dup
      Lorentz.sender
      if IsEq
      then nop
      else failUsing [mt|#fA2_NOT_OWNER|]
    checkAdmin OwnerOrOperatorTransfer = do
      dup
      Lorentz.sender
      if IsEq
      then nop
      else do
        dupTop2
        pair
        Lorentz.sender
        pair
        dig @3
        getField #sOperators
        dig @2
        mem
        if_ (dug @2) (failUsing [mt|#fA2_NOT_OPERATOR|])



debitFrom :: forall s. Storage & Address & (TokenId, Natural) & s :-> Storage & s
debitFrom = do
  dig @2
  unpair
  dig @3
  pair
  dig @2
  getField #sLedger
  dig @2
  dupTop2
  get
  ifSome ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists
      :: Natural & (Address, TokenId) & Ledger & Storage & Natural & s
      :-> Storage & s
    ifKeyExists = do
      dig @4
      rsub
      isNat
      ifSome
        (do
          some
          swap
          update
          setField #sLedger)
        (failUsing [mt|FA2_INSUFFICIENT_BALANCE|])

    ifKeyDoesntExist
      :: (Address, TokenId) & Ledger & Storage & Natural & s
      :-> Storage & s
    ifKeyDoesntExist = do
      dropN @2
      swap
      push @Natural 0
      if IsLt
      then failUsing [mt|FA2_INSUFFICIENT_BALANCE|]
      else nop

creditTo :: forall s. Storage & Address & (TokenId, Natural) & s :-> Storage & s
creditTo = do
  dig @2
  unpair
  dig @3
  pair
  dig @2
  getField #sLedger
  dig @2
  dupTop2
  get
  ifSome ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists
      :: Natural & (Address, TokenId) & Ledger & Storage & Natural & s
      :-> Storage & s
    ifKeyExists = do
      dig @4
      add
      some
      swap
      update
      setField #sLedger

    ifKeyDoesntExist
      :: (Address, TokenId) & Ledger & Storage & Natural & s
      :-> Storage & s
    ifKeyDoesntExist = do
      dig @3
      some
      swap
      update
      setField #sLedger


balanceOf :: Entrypoint BalanceRequestParams Storage
balanceOf = do
  checkedCoerce_
    @(FA2View "requests" [BalanceRequestItem] [BalanceResponseItem])
    @(View [BalanceRequestItem] [BalanceResponseItem])
  view_ checkAll
  where
    checkAll :: [BalanceRequestItem] & Storage & s :-> [BalanceResponseItem] & s
    checkAll = do
      nil @BalanceResponseItem
      swap
      iter checkOne
      swap
      drop @Storage

    checkOne :: forall s. BalanceRequestItem & [BalanceResponseItem] & Storage & s :-> [BalanceResponseItem] & Storage & s
    checkOne = do
      getField #briOwner
      swap
      getField #briTokenId
      swap
      dug @2
      dup
      push @Natural 0
      assertNeq [mt|#fA2_TOKEN_UNDEFINED|]
      swap
      pair
      dig @3
      getField #sLedger
      dig @2
      get
      swap
      dug @3
      ifSome ifKeyExists ifKeyDoesntExist

    ifKeyExists
      :: Natural & BalanceRequestItem & [BalanceResponseItem] & Storage & s
      :-> [BalanceResponseItem] & Storage & s
    ifKeyExists = do
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

    ifKeyDoesntExist
      :: BalanceRequestItem & [BalanceResponseItem] & Storage & s
      :-> [BalanceResponseItem] & Storage & s
    ifKeyDoesntExist = do
      push @Natural 0
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons


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
      ( #cAddOperator /-> addOperator
      , #cRemoveOperator /-> removeOperator
      )
  nil; pair

addOperator :: forall s. OperatorParam & Storage & s :-> Storage & s
addOperator = do
  getField #opTokenId
  dip do
    getField #opOperator
    dip $ toField #opOwner
  stackType @(TokenId & Address & Address & Storage & _)
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
  getField #opTokenId
  dip do
    getField #opOperator
    dip $ toField #opOwner
  stackType @(TokenId & Address & Address & Storage & _)
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