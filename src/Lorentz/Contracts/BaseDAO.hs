-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO
  ( module Exports

  , Ledger
  , Operators
  , Storage (..)

  , baseDaoContract
  , emptyStorage
  , mkStorage
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.FA2
import Lorentz.Contracts.Spec.FA2Interface as Exports
import Lorentz.Contracts.BaseDAO.Types

import Util.Markdown


contractDoc :: Markdown
contractDoc = [md|
  This documentation describes a smart contract which implements FA2 spec
  (https://gitlab.com/tzip/tzip/-/blob/667f925471728bb8e81fbd30b93a171e401b6dc1/proposals/tzip-12/tzip-12.md)
  |]


baseDaoContract :: Contract Parameter Storage
baseDaoContract = defaultContract $ contractName "FA2 smart contract" $ do
  doc $ DDescription contractDoc
  unpair
  entryCase @Parameter (Proxy @PlainEntrypointsKind)
    ( #cTransfer /-> transfer
    , #cBalance_of /-> balanceOf
    , #cToken_metadata_registry /-> tokenMetadataRegistry
    , #cPermissions_descriptor /-> permissionsDescriptor
    , #cUpdate_operators /-> updateOperators
    )

