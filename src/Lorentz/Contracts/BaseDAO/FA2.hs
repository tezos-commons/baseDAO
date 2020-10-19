-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.FA2
   ( transfer
   , balanceOf
   , tokenMetadataRegistry
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

transfer :: Entrypoint TransferParams Storage
transfer = do
  iter transferItem
  nil; pair
  where
    transferItem :: forall s. TransferItem & Storage & s :-> Storage & s
    transferItem = do
      swap
      getField #sAdmin
      Lorentz.sender
      if IsEq
      then push True
      else push False
      dig @2
      getField #tiTxs
      swap
      getField #tiFrom
      swap
      drop @TransferItem
      dug @2
      iter transferOne
      dropN @2

    transferOne :: forall s. TransferDestination & Bool & Address & Storage & s :-> Bool & Address & Storage & s
    transferOne = do
      swap
      dup
      if_ nop $ do
        dug @3
        dug @3
        checkSender
        dig @3
        dig @3
      dug @3
      getField #tdTo
      swap
      getField #tdAmount
      swap
      getField #tdTokenId
      swap
      drop @TransferDestination
      dup
      push @Natural 0
      if IsEq
      then nop
      else do
        dup
        push @Natural 1
        if IsEq
        then failCustom_ #fROZEN_TOKEN_NOT_TRANSFERABLE
        else failCustom_ #fA2_TOKEN_UNDEFINED
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
      dug @2
      swap

    checkSender
      :: forall f. Address & Storage & f
      :-> Address &  Storage & f
    checkSender = do
      dup
      Lorentz.sender
      if IsEq
      then nop
      else do
        dup
        dig @2
        getField #sOperators
        swap
        dug @3
        swap
        Lorentz.sender
        swap
        pair
        mem
        if_ nop (failCustom_ #fA2_NOT_OPERATOR)


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
      dupTop2
      rsub
      isNat
      ifSome
        (do
          some
          swap
          drop @Natural
          swap
          drop @Natural
          swap
          update
          setField #sLedger)
        (do
          toNamed #required
          swap
          toNamed #present
          swap
          pair
          failCustom #fA2_INSUFFICIENT_BALANCE)

    ifKeyDoesntExist
      :: (Address, TokenId) & Ledger & Storage & Natural & s
      :-> Storage & s
    ifKeyDoesntExist = do
      dropN @2
      swap
      push @Natural 0
      dupTop2
      if IsLt
      then do
        toNamed #present
        swap
        toNamed #required
        pair
        failCustom #fA2_INSUFFICIENT_BALANCE
      else (dropN @2)

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
      if IsEq
      then nop
      else do
        dup
        push @Natural 1
        if IsEq
        then nop
        else failCustom_ #fA2_TOKEN_UNDEFINED
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
  assertEq0or1
  swap
  dup
  Lorentz.sender
  if IsEq
  then nop
  else failCustom_ #nOT_OWNER
  swap
  pair
  swap
  getField #sOperators
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: (Address, Address) & Operators & Storage & s :-> Storage & s
    ifKeyExists = dropN @2

    ifKeyDoesntExist :: (Address, Address) & Operators & Storage & s :-> Storage & s
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
  assertEq0or1
  swap
  dup
  Lorentz.sender
  if IsEq
  then nop
  else failCustom_ #nOT_OWNER
  swap
  pair
  swap
  getField #sOperators
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: (Address, Address) & Operators & Storage & s :-> Storage & s
    ifKeyExists = do
      push Nothing
      swap
      update
      setField #sOperators


    ifKeyDoesntExist :: (Address, Address) & Operators & Storage & s :-> Storage & s
    ifKeyDoesntExist = dropN @2

-- | TODO: Probably this one can be moved to Lorentz as well
assertEq0or1 :: Natural & f :-> f
assertEq0or1 = do
  dup
  push @Natural 0
  if IsEq
  then drop @Natural
  else do
    dup
    push @Natural 1
    if IsEq
    then failUsing [mt|fA2_OPERATION_PROHIBITED|]
    else failCustom_ #fA2_TOKEN_UNDEFINED
