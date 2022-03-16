-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.BaseDAO.Types
  ( unit_TypesMatch
  ) where

import Universum

import Test.HUnit (Assertion)

import Morley.Michelson.Typed (Contract)

import Ligo.BaseDAO.Contract

unit_TypesMatch :: Assertion
unit_TypesMatch = evaluateNF_ @(Contract _ _) baseDAOContractLigo
