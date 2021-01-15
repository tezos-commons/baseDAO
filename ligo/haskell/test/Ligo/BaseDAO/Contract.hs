-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | LIGO version of the contract.
module Ligo.BaseDAO.Contract
  ( baseDAOContractLigo
  , l
  ) where

import Lorentz
import Michelson.Typed

import Ligo.BaseDAO.Types
import Ligo.Util

baseDAOContractLigo :: Michelson.Typed.Contract (ToT ParameterL) (ToT FullStorage)
baseDAOContractLigo =
  $(fetchContract @(ToT ParameterL) @(ToT FullStorage) "BASEDAO_LIGO_PATH")

-- l :: FullStorage -> Lambda (List Operation) (List Operation)
-- l st = push st # push zeroMutez # none # createContract baseDAOContractLigo # swap # drop # cons

l :: FullStorage -> Value (ToT (Lambda (List Operation) (List Operation)))
l st = VLam $ RfNormal $
  PUSH (toVal st) `Seq` PUSH (VMutez zeroMutez) `Seq` NONE `Seq`
  CREATE_CONTRACT baseDAOContractLigo `Seq`
  SWAP `Seq` DROP `Seq` CONS
