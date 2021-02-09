-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | LIGO version of the contract.
module Ligo.BaseDAO.Contract
  ( baseDAOContractLigo
  , baseDAOEntrypointsParameter
  ) where

import Michelson.Typed

import Ligo.BaseDAO.Types
import Ligo.Util

baseDAOContractLigo :: Contract (ToT ParameterL) (ToT FullStorage)
baseDAOContractLigo =
  $(fetchContract @(ToT ParameterL) @(ToT FullStorage) "BASEDAO_LIGO_PATH")

baseDAOEntrypointsParameter :: [StartupParameter]
baseDAOEntrypointsParameter =
  $(fetchValues @StartupParameter "ligo/haskell/test/entrypoints" "BASEDAO_LIGO_EPS_DIR_PATH")
