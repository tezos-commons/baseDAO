-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Proposal related entrypoints
module Lorentz.Contracts.BaseDAO.Permit
  ( toSignedData
  , permitSender
  , checkPermit
  , verifyPermit
  , verifyPermitProtected
  ) where

import Lorentz

import Lorentz.Contracts.BaseDAO.Types

incNonce :: Nonce : s :-> Nonce : s
incNonce = do
  forcedCoerce_ @Nonce @Natural
  push @Natural 1; add
  forcedCoerce_ @Natural @Nonce

-- | Wrap a data into @SignedData@.
--
-- You have to provide the original data and access to the respective
-- counter in storage.
toSignedData
  :: (StoreHasField store counterName Nonce, IsoValue d)
  => Label counterName -> d : store : s :-> DataToSign d : store : s
toSignedData counterLabel = do
  constructT
    ( fieldCtor chainId
    , fieldCtor selfAddress
    , fieldCtor $ do dip (stIncrement counterLabel); swap
    , fieldCtor dup
    )
  dip drop
  where
    stIncrement
      :: StoreHasField store l Nonce
      => Label l -> store : s :-> Nonce : store : s
    stIncrement l = do
      stGetField l
      dup
      dip $ do incNonce; stSetField l

-- | Get the implicit address of the author who signed the permit.
permitSender :: Permit a : s :-> Address : s
permitSender = do toField #pKey; hashKey; implicitAccount; address

-- | Check that given parameter fits permission.
--
-- On failure, this returns packed signed data for which we compared the
-- signature.
checkPermit
  :: NicePackedValue a
  => Permit a : DataToSign a : s :-> Either (Packed (DataToSign a)) () : s
checkPermit = do
  dip (pack # dup)
  getField #pKey; dip (toField #pSignature)
  checkSignature
  if Holds
  then do drop; push (Right ())
  else left

-- | Check that permit is signed by its author, and return the author
-- and the parameter to work with.
verifyPermit
  :: forall a store counterLabel s.
     (NicePackedValue a, StoreHasField store counterLabel Nonce)
  => Label counterLabel
  -> Permit a : a : store : s :-> a : ("author" :! Address) : store : s
verifyPermit counterLabel = do
  stackType @(Permit a : a : store : s)
  dupTop2
  dipN @2 $ do
    dip $ toSignedData counterLabel
    checkPermit
    if IsRight
    then drop @()
    else do checkedCoerce_; failCustom #mISSIGNED

  stackType @(Permit a : a : store : s)
  permitSender; toNamed #author
  swap

-- | Check that permit is signed by its author, and return the data
-- carried under permit protection.
verifyPermitProtected
  :: forall a store counterLabel s.
     (NicePackedValue a, StoreHasField store counterLabel Nonce)
  => Label counterLabel
  -> PermitProtected a : store : s :-> a : ("author" :! Address) : store : s
verifyPermitProtected counterLabel = do
  getField #ppPermit
  dip $ toField #ppArgument
  if IsNone
  then dip $ do sender; toNamed #author
  else verifyPermit counterLabel
