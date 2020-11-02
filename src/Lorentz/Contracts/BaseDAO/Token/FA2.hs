-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.Token.FA2
   ( transfer
   , balanceOf
   , tokenMetadataRegistry
   , updateOperators
   , defaultPermissionsDescriptor
   , debitFrom
   , creditTo
   ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Management (ensureNotMigrated)
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface

defaultPermissionsDescriptor :: PermissionsDescriptor
defaultPermissionsDescriptor = PermissionsDescriptor
  { pdOperator = OwnerOrOperatorTransfer
  , pdReceiver = OwnerNoHook
  , pdSender = OwnerNoHook
  , pdCustom = Nothing
  }

transfer :: forall pm. IsoValue pm =>  Entrypoint TransferParams (Storage pm)
transfer = do
  dip ensureNotMigrated
  iter transferItem
  nil; pair
  where
    transferItem :: '[TransferItem, Storage pm] :-> '[Storage pm]
    transferItem = do
      swap
      getField #sAdmin
      Lorentz.sender
      eq
      dig @2
      getField #tiTxs
      swap
      getField #tiFrom
      swap
      drop @TransferItem
      dug @2
      iter transferOne
      dropN @2

    transferOne :: '[TransferDestination, Bool, Address, Storage pm] :-> '[Bool, Address, Storage pm]
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
        then do
          dig @5
          dup
          if_ (dug @5) $
            failCustom_ #fROZEN_TOKEN_NOT_TRANSFERABLE
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
      :: forall f. Address : Storage pm : f
      :-> Address : Storage pm : f
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

debitFrom
  :: forall s store pm. StorageC store pm
  => store & Address & (TokenId, Natural) & s :-> store & s
debitFrom = do
  dig @2
  unpair
  dig @3
  pair
  dig @2
  stGetField #sLedger
  dig @2
  dupTop2
  get
  ifSome ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists
      :: Natural : (Address, TokenId) : Ledger : store : Natural : s
      :-> store : s
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
          stSetField #sLedger)
        (do
          toNamed #required
          swap
          toNamed #present
          swap
          pair
          failCustom #fA2_INSUFFICIENT_BALANCE)

    ifKeyDoesntExist
      :: (Address, TokenId) : Ledger : store : Natural : s
      :-> store : s
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

creditTo
  :: forall s pm store. (StorageC store pm)
  => store : Address : (TokenId, Natural) : s :-> store : s
creditTo = do
  dig @2
  unpair
  dig @3
  pair
  dig @2
  stGetField #sLedger
  dig @2
  dupTop2
  get
  ifSome ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists
      :: Natural : (Address, TokenId) : Ledger : store : Natural : s
      :-> store : s
    ifKeyExists = do
      dig @4
      add
      some
      swap
      update
      stSetField #sLedger

    ifKeyDoesntExist
      :: (Address, TokenId) : Ledger : store : Natural : s
      :-> store : s
    ifKeyDoesntExist = do
      dig @3
      some
      swap
      update
      stSetField #sLedger


balanceOf :: forall pm. IsoValue pm => Entrypoint BalanceRequestParams (Storage pm)
balanceOf = do
  dip ensureNotMigrated
  checkedCoerce_
    @(FA2View "requests" [BalanceRequestItem] [BalanceResponseItem])
    @(View [BalanceRequestItem] [BalanceResponseItem])
  view_ checkAll
  where
    checkAll :: [BalanceRequestItem] : Storage pm : s :-> [BalanceResponseItem] : s
    checkAll = do
      nil @BalanceResponseItem
      swap
      iter checkOne
      swap
      drop @(Storage pm)

    checkOne :: forall s. BalanceRequestItem : [BalanceResponseItem] : Storage pm : s :-> [BalanceResponseItem] : Storage pm : s
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
      :: Natural : BalanceRequestItem : [BalanceResponseItem] : Storage pm : s
      :-> [BalanceResponseItem] : Storage pm : s
    ifKeyExists = do
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

    ifKeyDoesntExist
      :: BalanceRequestItem : [BalanceResponseItem] : Storage pm : s
      :-> [BalanceResponseItem] : Storage pm : s
    ifKeyDoesntExist = do
      push @Natural 0
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

tokenMetadataRegistry :: IsoValue pm => Entrypoint TokenMetadataRegistryParam (Storage pm)
tokenMetadataRegistry = do
  dip ensureNotMigrated
  swap
  getField #sTokenAddress
  dig @2
  swap
  push @Mutez (toMutez 0)
  swap
  transferTokens # nil # swap # cons # pair

updateOperators :: IsoValue pm => Entrypoint UpdateOperatorsParam (Storage pm)
updateOperators = do
  dip ensureNotMigrated
  iter $
    caseT @UpdateOperator
      ( #cAddOperator /-> addOperator
      , #cRemoveOperator /-> removeOperator
      )
  nil; pair

addOperator :: forall pm. IsoValue pm => '[OperatorParam, Storage pm] :-> '[Storage pm]
addOperator = do
  getField #opTokenId
  dip do
    getField #opOperator
    dip $ toField #opOwner
  stackType @(TokenId : Address : Address : (Storage pm) : _)
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
    ifKeyExists :: '[(Address, Address), Operators, Storage pm] :-> '[Storage pm]
    ifKeyExists = dropN @2

    ifKeyDoesntExist ::  '[(Address, Address), Operators, Storage pm] :-> '[Storage pm]
    ifKeyDoesntExist = do
      unit
      some
      swap
      update
      setField #sOperators

removeOperator :: forall pm. IsoValue pm => '[OperatorParam, Storage pm] :-> '[Storage pm]
removeOperator = do
  getField #opTokenId
  dip do
    getField #opOperator
    dip $ toField #opOwner
  stackType @(TokenId : Address : Address : (Storage pm) : _)
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
    ifKeyExists ::  '[(Address, Address), Operators, Storage pm] :-> '[Storage pm]
    ifKeyExists = do
      push Nothing
      swap
      update
      setField #sOperators


    ifKeyDoesntExist ::  '[(Address, Address), Operators, Storage pm] :-> '[Storage pm]
    ifKeyDoesntExist = dropN @2

-- | TODO: Probably this one can be moved to Lorentz as well
assertEq0or1 :: Natural : f :-> f
assertEq0or1 = do
  dup
  push @Natural 0
  if IsEq
  then drop @Natural
  else do
    dup
    push @Natural 1
    if IsEq
    then failUsing [mt|OPERATION_PROHIBITED|]
    else failCustom_ #fA2_TOKEN_UNDEFINED
