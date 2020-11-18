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
import Lorentz.Contracts.BaseDAO.Doc(burnDoc, mintDoc, transferContractTokensDoc, tokenAddressDoc)
import Lorentz.Contracts.BaseDAO.Management (authorizeAdmin, ensureNotMigrated)
import Lorentz.Contracts.BaseDAO.Token.FA2 (creditTo, debitFrom)
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

burn
  :: forall store pm s. (StorageC store pm, HasFuncContext s store)
  => Entrypoint' BurnParam store s
burn = do
  doc $ DDescription burnDoc
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

  callCachedFunc debitFrom
  nil; pair

mint
  :: forall store pm s. (StorageC store pm, HasFuncContext s store)
  => Entrypoint' MintParam store s
mint = do
  doc $ DDescription mintDoc
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

  callCachedFunc creditTo
  nil; pair

transferContractTokens
  :: forall store pm s. StorageC store pm
  => Entrypoint' TransferContractTokensParam store s
transferContractTokens = do
  doc $ DDescription transferContractTokensDoc
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
  :: forall store pm s. (HasAnnotation pm, NiceParameter pm, StorageC store pm)
  => Entrypoint' TokenAddressParam store s
tokenAddress = do
  doc $ DDescription tokenAddressDoc
  dip $ do
    ensureNotMigrated
  stackType @(TokenAddressParam : store : s)
  push zeroMutez
  self @(Parameter pm); address
  transferTokens # nil # swap # cons # pair
