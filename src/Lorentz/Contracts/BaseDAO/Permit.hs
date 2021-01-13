-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Proposal related entrypoints
module Lorentz.Contracts.BaseDAO.Permit
  ( toSignedData
  , permitSender
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
checkedPermitSender
  :: NicePackedValue a
  => Permit a : DataToSign a : s :-> Address : s
checkedPermitSender = do
  dip pack
  dupTop2
  getField #pKey; dip (toField #pSignature)
  checkSignature
  if Holds
  then do dip (drop @(Packed $ DataToSign _)); permitSender
  else do drop @(Permit _); checkedCoerce_; failCustom #mISSIGNED

-- | Check that permit is signed by its author, and return the author
-- and the parameter to work with.
verifyPermit
  :: forall a store counterLabel s.
     (NicePackedValue a, StoreHasField store counterLabel Nonce)
  => Label counterLabel
  -> Permit a : a : store : s :-> a : ("author" :! Address) : store : s
verifyPermit counterLabel = do
  duupX @2
  dip $ do
    dip $ toSignedData counterLabel
    checkedPermitSender
    toNamed #author

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
