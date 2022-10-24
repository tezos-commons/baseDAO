-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Plist.Contract
  ( plistContractLigo
  ) where

import Lorentz qualified as L
import Test.Cleveland.Lorentz (embedContract)

import Test.Plist.Type

plistContractLigo :: L.Contract PlistParameter PlistStorage ()
plistContractLigo =
  $$(embedContract @PlistParameter @PlistStorage @() "resources/plist_contract.tz")
