-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Plist.Contract
  ( plistContractLigo
  ) where

import Test.Cleveland.Lorentz (embedContract)
import Morley.Michelson.Typed
import qualified Lorentz as L

import Test.Plist.Type

plistContractLigo :: Contract (ToT PlistParameter) (ToT PlistStorage)
plistContractLigo = L.toMichelsonContract
  $$(embedContract @PlistParameter @PlistStorage "resources/plist_contract.tz")

