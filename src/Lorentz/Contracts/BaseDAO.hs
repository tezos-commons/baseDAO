-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO
  ( Ledger
  , Operators
  , Parameter (..)
  , Storage (..)

  , baseDaoContract
  , emptyStorage
  , mkStorage
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.FA2
import Lorentz.Contracts.BaseDAO.Management
import Lorentz.Contracts.BaseDAO.Types as BaseDAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Util.Markdown

contractDoc :: Markdown
contractDoc = [md|
  This documentation describes a smart contract which implements FA2 spec
  (https://gitlab.com/tzip/tzip/-/blob/667f925471728bb8e81fbd30b93a171e401b6dc1/proposals/tzip-12/tzip-12.md)
  |]

baseDaoContract :: Contract BaseDAO.Parameter Storage
baseDaoContract = defaultContract $ contractName "FA2 smart contract" $ do
  doc $ DDescription contractDoc
  ensureZeroTransfer
  unpair
  entryCase @BaseDAO.Parameter (Proxy @PlainEntrypointsKind)
    ( #cCall_FA2 /-> fa2Handler
    , #cTransfer_ownership /-> transferOwnership
    , #cAccept_ownership /-> acceptOwnership
    , #cMigrate /-> migrate
    , #cConfirm_migration /-> confirmMigration
    )

fa2Handler :: Entrypoint FA2.Parameter Storage
fa2Handler =
  entryCase @FA2.Parameter (Proxy @PlainEntrypointsKind)
    ( #cTransfer /-> transfer
    , #cBalance_of /-> balanceOf
    , #cToken_metadata_registry /-> tokenMetadataRegistry
    , #cUpdate_operators /-> updateOperators
    )

ensureZeroTransfer :: s :-> s
ensureZeroTransfer = do
  amount
  push zeroMutez
  if IsEq then nop else failCustom_ #fORBIDDEN_XTZ
