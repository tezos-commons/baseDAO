-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.Model.BaseDAO.Token
  ( makeTransferOnToken
  , applyTransferContractTokens
  ) where

import Universum

import Control.Monad.Except (throwError)
import qualified Data.Map as Map

import Lorentz hiding (not, cast, checkSignature, get)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types

makeTransferOnToken :: FA2.TransferParams -> Address -> ModelT ()
makeTransferOnToken params addr = do
  ms <- get
  case Map.lookup addr (ms & msContracts) of
    Just (SimpleFA2ContractType _) -> pass
    _ -> throwError BAD_TOKEN_CONTRACT
  execOperation $ FA2TransferOperation addr params (toMutez 0)

applyTransferContractTokens :: ModelSource -> TransferContractTokensParam -> ModelT ()
applyTransferContractTokens mso param = do

  store <- getStore
  unless (msoSender mso == sAdmin store) $ throwError NOT_ADMIN

  makeTransferOnToken (param & tcParams) (param & tcContractAddress)
