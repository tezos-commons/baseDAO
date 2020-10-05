-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Basic
  ( unit_can_be_printed
  ) where

import Universum

import Lorentz
import Test.Tasty.HUnit (Assertion)

import Lorentz.Contracts.BaseDAO (baseDaoContract)

-- | Stub test to be removed later.
unit_can_be_printed :: Assertion
unit_can_be_printed = putStrLn (printLorentzContract True baseDaoContract )
