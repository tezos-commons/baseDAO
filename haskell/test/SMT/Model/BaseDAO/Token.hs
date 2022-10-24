-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.Model.BaseDAO.Token
  ( makeTransferOnToken
  , applyTransferContractTokens
  ) where

import Universum

import Control.Monad.Except (throwError)
import Data.Map qualified as Map

import Lorentz hiding (cast, checkSignature, get, not)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types

makeTransferOnToken :: FA2.TransferParams -> Address -> ModelT cep ()
makeTransferOnToken params addr = do
  ms <- get
  case Map.lookup addr (ms & msContracts) of
    Just (SimpleFA2ContractType _) -> pass
    _ -> throwError BAD_TOKEN_CONTRACT
  execOperation $ FA2TransferOperation addr params zeroMutez

applyTransferContractTokens :: ModelSource -> TransferContractTokensParam -> ModelT cep ()
applyTransferContractTokens mso param = do

  store <- getStore
  unless (toAddress (msoSender mso) == sAdmin store) $ throwError NOT_ADMIN

  makeTransferOnToken (param & tcParams) (param & tcContractAddress)
