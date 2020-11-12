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
import Lorentz.Contracts.BaseDAO.Doc (transferDoc, balanceOfDoc, tokenMetadataRegistryDoc, updateOperatorsDoc)
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

transfer
  :: forall pm s. (IsoValue pm, HasFuncContext s (Storage pm))
  => Entrypoint' TransferParams (Storage pm) s
transfer = do
  doc $ DDescription transferDoc
  dip ensureNotMigrated
  iter transferItem
  nil; pair
  where
    transferItem :: (TransferItem : Storage pm : s) :-> (Storage pm : s)
    transferItem = do
      swap
      getField #sAdmin
      Lorentz.sender
      eq
      dig @2
      getField #tiTxs
      swap
      getField #tiFrom; toNamed #from
      swap
      drop @TransferItem
      dug @2
      iter transferOne
      dropN @2

    transferOne :: TransferDestination : Bool : "from" :! Address : Storage pm : s :-> Bool : "from" :! Address : Storage pm : s
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
      dug @2; fromNamed #from
      dig @4
      callCachedFunc debitFrom;
      callCachedFunc creditTo;
      dug @2
      swap

    checkSender
      :: forall f. ("from" :! Address) : Storage pm : f
      :-> ("from" :! Address) : Storage pm : f
    checkSender = do
      dup; fromNamed #from
      Lorentz.sender
      if IsEq
      then nop
      else do
        dup
        dig @2
        getField #sOperators
        swap
        dug @3
        swap; fromNamed #from; toNamed #owner
        Lorentz.sender; toNamed #operator
        swap
        pair
        mem
        if_ nop (failCustom_ #fA2_NOT_OPERATOR)

debitFrom
  :: forall store pm. (StorageC store pm, IsoValue store)
  => CachedFunc "debitFrom" [store, Address, (TokenId, Natural)] '[store]
debitFrom = mkCachedFunc $ do
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
      :: '[Natural, (Address, TokenId), Ledger, store, Natural]
      :-> '[store]
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
      :: [(Address, TokenId), Ledger, store, Natural]
      :-> '[store]
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
  :: forall pm store. (StorageC store pm, IsoValue store)
  => CachedFunc "creditTo" [store, Address, (TokenId, Natural)] '[store]
creditTo = mkCachedFunc $ do
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
      :: Natural : (Address, TokenId) : Ledger : store : Natural : '[]
      :-> store : '[]
    ifKeyExists = do
      dig @4
      add
      some
      swap
      update
      stSetField #sLedger

    ifKeyDoesntExist
      :: (Address, TokenId) : Ledger : store : Natural : '[]
      :-> store : '[]
    ifKeyDoesntExist = do
      dig @3
      some
      swap
      update
      stSetField #sLedger


balanceOf
  :: forall pm s. IsoValue pm
  => Entrypoint' BalanceRequestParams (Storage pm) s
balanceOf = do
  doc $ DDescription balanceOfDoc
  dip ensureNotMigrated
  checkedCoerce_
    @(FA2View "requests" [BalanceRequestItem] [BalanceResponseItem])
    @(View [BalanceRequestItem] [BalanceResponseItem])
  view_ checkAll
  where
    checkAll :: [BalanceRequestItem] : Storage pm : s0 :-> [BalanceResponseItem] : s0
    checkAll = do
      nil @BalanceResponseItem
      swap
      iter checkOne
      swap
      drop @(Storage pm)

    checkOne
      :: BalanceRequestItem : [BalanceResponseItem] : Storage pm : s0
      :-> [BalanceResponseItem] : Storage pm : s0
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
      :: Natural : BalanceRequestItem : [BalanceResponseItem] : Storage pm : s0
      :-> [BalanceResponseItem] : Storage pm : s0
    ifKeyExists = do
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

    ifKeyDoesntExist
      :: BalanceRequestItem : [BalanceResponseItem] : Storage pm : s0
      :-> [BalanceResponseItem] : Storage pm : s0
    ifKeyDoesntExist = do
      push @Natural 0
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

tokenMetadataRegistry :: IsoValue pm => Entrypoint' TokenMetadataRegistryParam (Storage pm) s
tokenMetadataRegistry = do
  doc $ DDescription tokenMetadataRegistryDoc
  dip ensureNotMigrated
  swap
  getField #sTokenAddress
  dig @2
  swap
  push @Mutez (toMutez 0)
  swap
  transferTokens # nil # swap # cons # pair

updateOperators :: IsoValue pm => Entrypoint' UpdateOperatorsParam (Storage pm) s
updateOperators = do
  doc $ DDescription updateOperatorsDoc
  dip ensureNotMigrated
  iter $
    caseT @UpdateOperator
      ( #cAddOperator /-> addOperator
      , #cRemoveOperator /-> removeOperator
      )
  nil; pair

addOperator :: forall pm s. IsoValue pm => (OperatorParam : Storage pm : s) :-> (Storage pm : s)
addOperator = do
  getField #opTokenId
  dip do
    getField #opOperator; toNamed #operator
    dip $ do toField #opOwner; toNamed #owner
  stackType @(TokenId : "operator" :! Address : "owner" :! Address : Storage pm : s)
  assertEq0or1
  swap
  dup; fromNamed #owner
  Lorentz.sender
  if IsEq
  then nop
  else failCustom_ #nOT_OWNER
  stackType @("owner" :! Address : "operator" :! Address : Storage pm : s)
  pair
  swap
  getField #sOperators
  stackType @( BigMap ("owner" :! Address, "operator" :! Address) ()
             : Storage pm : ("owner" :! Address, "operator" :! Address) : s)
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: ("owner" :! Address, "operator" :! Address) : Operators : Storage pm : s :-> Storage pm : s
    ifKeyExists = dropN @2

    ifKeyDoesntExist :: ("owner" :! Address, "operator" :! Address) : Operators : Storage pm : s :-> Storage pm : s
    ifKeyDoesntExist = do
      unit
      some
      swap
      update
      setField #sOperators

removeOperator :: forall pm s. IsoValue pm => (OperatorParam : Storage pm : s) :-> (Storage pm : s)
removeOperator = do
  getField #opTokenId
  dip do
    getField #opOperator; toNamed #operator
    dip $ do toField #opOwner; toNamed #owner
  stackType @(TokenId : "operator" :! Address : "owner" :! Address : Storage pm : s)
  assertEq0or1
  swap
  dup; fromNamed #owner
  Lorentz.sender
  if IsEq
  then nop
  else failCustom_ #nOT_OWNER
  stackType @("owner" :! Address : "operator" :! Address : Storage pm : s)
  pair
  swap
  getField #sOperators
  stackType @( BigMap ("owner" :! Address, "operator" :! Address) ()
             : Storage pm : ("owner" :! Address, "operator" :! Address) : s)
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: ("owner" :! Address, "operator" :! Address) : Operators : Storage pm : s :-> Storage pm : s
    ifKeyExists = do
      push Nothing
      swap
      update
      setField #sOperators


    ifKeyDoesntExist :: ("owner" :! Address, "operator" :! Address) : Operators : Storage pm : s :-> Storage pm : s
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
