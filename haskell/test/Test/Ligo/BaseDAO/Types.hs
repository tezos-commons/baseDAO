-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Types
  ( unit_TypesMatch
  ) where

import Universum

import Test.HUnit (Assertion)

import Michelson.Typed (Contract)

import Ligo.BaseDAO.Contract

unit_TypesMatch :: Assertion
unit_TypesMatch = evaluateNF_ @(Contract _ _) baseDAOContractLigo
