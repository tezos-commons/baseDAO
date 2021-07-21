-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.Model.BaseDAO.Management
  ( applyAcceptOwnership
  , applyTransferOwnership
  , applyCallCustom
  ) where

import Universum

import Control.Monad.Except (throwError)
import qualified Data.Map as Map

import Lorentz hiding (not, cast, checkSignature, get)

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types

applyTransferOwnership :: ModelSource -> TransferOwnershipParam -> ModelT ()
applyTransferOwnership mso param = do
  selfAddr <- get <&> msSelfAddress

  modifyStore $ \s -> do
    let newOwner = arg #newOwner param

    unless (msoSender mso == sAdmin s) $
      throwError NOT_ADMIN

    if selfAddr == newOwner then
      pure $ s { sAdmin = newOwner }
    else
      pure $ s { sPendingOwner = newOwner}

applyAcceptOwnership :: ModelSource -> ModelT ()
applyAcceptOwnership mso = modifyStore $ \s ->
  if (s & sPendingOwner) == (mso & msoSender) then
    pure $ s { sAdmin = (mso & msoSender)}
  else throwError NOT_PENDING_ADMIN

applyCallCustom :: ModelSource -> CallCustomParam -> ModelT ()
applyCallCustom _ (epName, packedParam) = do
  customEps <- get <&> msCustomEps
  case Map.lookup epName customEps of
    Just f ->
      f packedParam
    Nothing ->
      throwError ENTRYPOINT_NOT_FOUND
