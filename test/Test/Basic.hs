-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Basic
  ( unit_updates_storage_properly
  ) where

import Basic (Parameter(..), measureBoaConstrictor)

import Lorentz
import Lorentz.Test
import Michelson.Test.Integrational
import Michelson.Typed.Convert
import Test.Tasty.HUnit (Assertion)

unit_updates_storage_properly :: Assertion
unit_updates_storage_properly = integrationalTestExpectation $ do
  contractAddr <- originate (convertContract $ compileLorentzContract measureBoaConstrictor) "basic"
          (untypeValue $ toVal (10 :: Integer)) zeroMutez

  let
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = TxTypedParam $ toVal $ Zero ()
      , tdEntrypoint = DefEpName
      , tdAmount = zeroMutez
      }

  transfer txData contractAddr

  expectStorageConst contractAddr
    $ untypeValue $ toVal (0 :: Integer)
