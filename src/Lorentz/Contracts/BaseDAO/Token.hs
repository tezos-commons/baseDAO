-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Token related entrypoints that are not part of FA2
module Lorentz.Contracts.BaseDAO.Token
  ( burn
  , mint
  , transferContractTokens
  , tokenAddress
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Management (authorizeAdmin, ensureNotMigrated)
import Lorentz.Contracts.BaseDAO.Token.FA2 (debitFrom, creditTo)
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

burn
  :: forall store pm. StorageC store pm
  => Entrypoint BurnParam store
burn = do
  dip $ do
    ensureNotMigrated
    authorizeAdmin
  swap
  dip $ do
    getField #bTokenId
    dip $ getField #bAmount
    pair
    swap
    toField #bFrom_

  debitFrom
  nil; pair

mint
  :: forall store pm. StorageC store pm
  => Entrypoint MintParam store
mint = do
  dip $ do
    ensureNotMigrated
    authorizeAdmin
  swap
  dip $ do
    getField #mTokenId
    dip $ getField #mAmount
    pair
    swap
    toField #mTo_

  creditTo
  nil; pair

transferContractTokens
  :: forall store pm. StorageC store pm
  => Entrypoint TransferContractTokensParam store
transferContractTokens = do
  dip $ do
    authorizeAdmin

  getField #tcParams
  dip $ do
    toField #tcContractAddress
    contractCalling @FA2.Parameter (Call @"Transfer")
    ifSome nop $ do
      failCustom_ #fAIL_TRANSFER_CONTRACT_TOKENS
    push zeroMutez
  transferTokens
  nil; swap; cons
  pair

tokenAddress
  :: forall store pm. (HasAnnotation pm, NiceParameter pm, StorageC store pm)
  => Entrypoint TokenAddressParam store
tokenAddress = do
  dip $ do
    ensureNotMigrated
  stackType @'[TokenAddressParam, store]
  push zeroMutez
  self @(Parameter pm); address
  transferTokens # nil # swap # cons # pair