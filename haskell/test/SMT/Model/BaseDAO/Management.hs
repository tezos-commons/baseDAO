-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.Model.BaseDAO.Management
  ( applyAcceptOwnership
  , applyTransferOwnership
  , applyCallCustom
  ) where

import Universum

import Control.Monad.Except (throwError)

import Lorentz.Address
import Morley.Util.Named

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types

applyTransferOwnership :: ModelSource -> TransferOwnershipParam -> ModelT cep ()
applyTransferOwnership mso (arg #newOwner -> param) = do
  selfAddr <- get <&> msSelfAddress

  modifyStore $ \s -> do
    let newOwner = param

    unless (toAddress (msoSender mso) == sAdmin s) $
      throwError NOT_ADMIN

    if selfAddr == newOwner then
      pure $ s { sAdmin = newOwner }
    else
      pure $ s { sPendingOwner = newOwner}

applyAcceptOwnership :: ModelSource -> ModelT cep ()
applyAcceptOwnership mso = modifyStore $ \s ->
  if (s & sPendingOwner) == toAddress (mso & msoSender) then
    pure $ s { sAdmin = toAddress (mso & msoSender)}
  else throwError NOT_PENDING_ADMIN

applyCallCustom :: ModelSource -> (VariantToParam var) -> ModelT var ()
applyCallCustom _ cep = do
  customEps <- get <&> msCustomEps
  customEps cep
