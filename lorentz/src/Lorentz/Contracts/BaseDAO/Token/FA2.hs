-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.Token.FA2
   ( transfer
   , balanceOf
   , updateOperators
   , defaultPermissionsDescriptor
   , debitFrom
   , creditTo

     -- * Helpers
   , convertOperatorParam
   ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Doc (balanceOfDoc, transferDoc, updateOperatorsDoc)
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
  :: forall ce pm s.
     (IsoValue ce, KnownValue pm, HasFuncContext s (Storage ce pm))
  => Entrypoint' TransferParams (Storage ce pm) s
transfer = do
  doc $ DDescription transferDoc
  dip ensureNotMigrated
  iter transferItem
  nil; pair
  where
    transferItem :: (TransferItem : Storage ce pm : s) :-> (Storage ce pm : s)
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

    transferOne :: TransferDestination : Bool : "from" :! Address : Storage ce pm : s :-> Bool : "from" :! Address : Storage ce pm : s
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
      duupX @6
      stGetField #sUnfrozenTokenId
      dip swap
      if IsEq
      then drop
      else do
        dip dup
        stToField #sFrozenTokenId
        if IsEq
        then do
          dig @5
          dup
          if_ (dug @5) $
            failCustomNoArg #fROZEN_TOKEN_NOT_TRANSFERABLE
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
      :: forall f. ("from" :! Address) : Storage ce pm : f
      :-> ("from" :! Address) : Storage ce pm : f
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


increaseTotalSupply
  :: forall store ce pm s. (StorageC store ce pm)
  => TokenId : Natural : store : s :-> store : s
increaseTotalSupply = do
  duupX @3; duupX @2
  stackType @(TokenId : store : TokenId : Natural : store : s)
  stGet #sTotalSupply
  -- TokenId should always be presented in the map
  ifSome nop (failCustom_ #fA2_TOKEN_UNDEFINED)

  stackType @(Natural : TokenId : Natural : store : s)
  dip swap; add
  some
  swap; stUpdate #sTotalSupply


decreaseTotalSupply
  :: forall store ce pm s. (StorageC store ce pm)
  => TokenId : Natural : store : s :-> store : s
decreaseTotalSupply = do
  duupX @3; duupX @2
  stackType @(TokenId : store : TokenId : Natural : store : s)
  stGet #sTotalSupply
  -- TokenId should always be presented in the map
  ifSome nop (failCustom_ #fA2_TOKEN_UNDEFINED)

  stackType @(Natural : TokenId : Natural : store : s)
  dip swap; sub; isNat
  -- Decreasing total_supply below 0 should never happen
  ifSome nop (failCustom_ #nEGATIVE_TOTAL_SUPPLY)

  some
  swap; stUpdate #sTotalSupply

debitFrom
  :: forall store ce pm. (StorageC store ce pm, IsoValue store)
  => DebitFromFunc store
debitFrom = mkCachedFunc $ do
  dip do
    dip unpair
    pair
    dip $ toNamed #required
  stackType @[store, (Address, TokenId), "required" :! Natural]
  dup; duupX @3
  stGet #sLedger

  ifSome ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists
      :: '[Natural, store, (Address, TokenId), "required" :! Natural]
      :-> '[store]
    ifKeyExists = do
      -- Ensure total_supply is properly decreased
      swap
      duupX @4; fromNamed #required
      duupX @4; cdr
      decreaseTotalSupply; swap

      stackType @[Natural, store, (Address, TokenId), "required" :! Natural]
      duupX @4; fromNamed #required; duupX @2
      sub; isNat; dip $ toNamed #present

      stackType @[Maybe Natural, "present" :! Natural, store, (Address, TokenId), "required" :! Natural]

      -- Ensure sufficient balance
      ifSome
        (do
          dip (drop @("present" :! Natural))
          some; dip swap; swap; stUpdate #sLedger
          dip (drop @( "required" :! Natural))
        )
        (do
          dip (dropN @2); swap; pair
          failCustom #fA2_INSUFFICIENT_BALANCE
        )

    ifKeyDoesntExist
      :: [store, (Address, TokenId), "required" :! Natural]
      :-> '[store]
    ifKeyDoesntExist = do
      dip drop; swap
      dup; fromNamed #required; push @Natural 0
      if IsEq then
        drop
      else do
        push @Natural 0; toNamed #present
        swap; pair
        failCustom #fA2_INSUFFICIENT_BALANCE

creditTo
  :: forall pm ce store. (StorageC store ce pm, IsoValue store)
  => CreditToFunc store
creditTo = mkCachedFunc $ do
  dip do
    dip unpair
    pair
    dip $ toNamed #updated

  stackType @[store, (Address, TokenId), "updated" :! Natural]

  -- Ensure total_supply is properly increased
  duupX @3; fromNamed #updated
  duupX @3; cdr
  increaseTotalSupply
  stackType @[store, (Address, TokenId), "updated" :! Natural]

  dup; duupX @3
  stGet #sLedger

  ifSome ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists
      :: '[Natural, store, (Address, TokenId), "updated" :! Natural]
      :-> '[store]
    ifKeyExists = do
      dig @3; fromNamed #updated; add

      stackType @[Natural, store, (Address, TokenId)]
      some; dip swap; swap; stUpdate #sLedger


    ifKeyDoesntExist
      :: [store, (Address, TokenId), "updated" :! Natural]
      :-> '[store]
    ifKeyDoesntExist = do
      swap; dig @2; fromNamed #updated
      some; swap
      stUpdate #sLedger

balanceOf
  :: forall ce pm s. (IsoValue ce, KnownValue pm)
  => Entrypoint' BalanceRequestParams (Storage ce pm) s
balanceOf = do
  doc $ DDescription balanceOfDoc
  dip ensureNotMigrated
  checkedCoerce_
    @(FA2View "requests" [BalanceRequestItem] [BalanceResponseItem])
    @(View [BalanceRequestItem] [BalanceResponseItem])
  view_ checkAll
  where
    checkAll :: [BalanceRequestItem] : Storage ce pm : s0 :-> [BalanceResponseItem] : s0
    checkAll = do
      nil @BalanceResponseItem
      swap
      iter checkOne
      swap
      drop @(Storage ce pm)

    checkOne
      :: BalanceRequestItem : [BalanceResponseItem] : Storage ce pm : s0
      :-> [BalanceResponseItem] : Storage ce pm : s0
    checkOne = do
      getField #briOwner
      swap
      getField #briTokenId
      swap
      dug @2
      dup
      duupX @6
      stGetField #sUnfrozenTokenId
      dip swap
      if IsEq
      then drop
      else do
        dip dup
        stToField #sFrozenTokenId
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
      :: Natural : BalanceRequestItem : [BalanceResponseItem] : Storage ce pm : s0
      :-> [BalanceResponseItem] : Storage ce pm : s0
    ifKeyExists = do
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

    ifKeyDoesntExist
      :: BalanceRequestItem : [BalanceResponseItem] : Storage ce pm : s0
      :-> [BalanceResponseItem] : Storage ce pm : s0
    ifKeyDoesntExist = do
      push @Natural 0
      swap
      constructStack @BalanceResponseItem @[BalanceRequestItem, Natural]
      cons

updateOperators
  :: (IsoValue ce, KnownValue pm)
  => Entrypoint' UpdateOperatorsParam (Storage ce pm) s
updateOperators = do
  doc $ DDescription updateOperatorsDoc
  dip ensureNotMigrated
  iter $
    caseT @UpdateOperator
      ( #cAddOperator /-> addOperator
      , #cRemoveOperator /-> removeOperator
      )
  nil; pair

addOperator
  :: forall ce pm s.
     (IsoValue ce, KnownValue pm)
  => (OperatorParam : Storage ce pm : s) :-> (Storage ce pm : s)
addOperator = do
  dip dup
  convertOperatorParam
  dup; fromNamed #owner
  Lorentz.sender
  if IsEq
  then nop
  else failCustomNoArg #nOT_OWNER
  stackType @("owner" :! Address : "operator" :! Address : Storage ce pm : s)
  pair
  swap
  getField #sOperators
  stackType @( BigMap ("owner" :! Address, "operator" :! Address) ()
             : Storage ce pm : ("owner" :! Address, "operator" :! Address) : s)
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: ("owner" :! Address, "operator" :! Address) : Operators : Storage ce pm : s :-> Storage ce pm : s
    ifKeyExists = dropN @2

    ifKeyDoesntExist :: ("owner" :! Address, "operator" :! Address) : Operators : Storage ce pm : s :-> Storage ce pm : s
    ifKeyDoesntExist = do
      unit
      some
      swap
      update
      setField #sOperators

removeOperator
  :: forall ce pm s.
     (IsoValue ce, KnownValue pm)
  => (OperatorParam : Storage ce pm : s) :-> (Storage ce pm : s)
removeOperator = do
  dip dup
  convertOperatorParam
  dup; fromNamed #owner
  Lorentz.sender
  if IsEq
  then nop
  else failCustomNoArg #nOT_OWNER
  stackType @("owner" :! Address : "operator" :! Address : Storage ce pm : s)
  pair
  swap
  getField #sOperators
  stackType @( BigMap ("owner" :! Address, "operator" :! Address) ()
             : Storage ce pm : ("owner" :! Address, "operator" :! Address) : s)
  dig @2
  dupTop2
  mem
  if_ ifKeyExists ifKeyDoesntExist
  where
    ifKeyExists :: ("owner" :! Address, "operator" :! Address) : Operators : Storage ce pm : s :-> Storage ce pm : s
    ifKeyExists = do
      push Nothing
      swap
      update
      setField #sOperators


    ifKeyDoesntExist :: ("owner" :! Address, "operator" :! Address) : Operators : Storage ce pm : s :-> Storage ce pm : s
    ifKeyDoesntExist = dropN @2

convertOperatorParam
  :: ( StoreHasField store "sUnfrozenTokenId" TokenId
     , StoreHasField store "sFrozenTokenId" TokenId
     )
  => OperatorParam : store : s
  :-> ("owner" :! Address) : ("operator" :! Address) : s
convertOperatorParam = do
  getField #opTokenId
  dip do
    getField #opOperator; toNamed #operator
    dip $ do toField #opOwner; toNamed #owner
    dig @2
  validateOperatorToken
  swap

validateOperatorToken
  :: forall store f.
     ( StoreHasField store "sUnfrozenTokenId" TokenId
     , StoreHasField store "sFrozenTokenId" TokenId
     )
  => TokenId : store : f :-> f
validateOperatorToken = do
  dip (stGetField #sUnfrozenTokenId)
  dup
  dip swap
  if IsEq
  then dropN @2 @(TokenId : store : f)
  else do
    dip (stToField #sFrozenTokenId)
    if IsEq
    then failUsing [mt|OPERATION_PROHIBITED|]
    else failCustom_ #fA2_TOKEN_UNDEFINED
