-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Read a contract at compile time.
module Ligo.Util
  ( fetchContract
  ) where

import Universum

import Fmt (pretty)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import System.Environment (lookupEnv)

import Michelson.Test.Import
import Michelson.Typed

-- | Read a contract at compile time assuming its expected type is known.
--
-- This is not an ideal implementation, e.g. it does not pretty-print
-- types in error messages on types mismatch.
fetchContract :: forall cp st. (KnownT cp, KnownT st) => String -> TH.ExpQ
fetchContract envKey = do
  path <- fromMaybe "ligo/test/baseDAO.tz" <$> liftIO (lookupEnv envKey)
          -- ↑ This default path works on CI.
          -- There it's relative to the repo root, apparently.

  -- We cannot use 'embedFile' directly because it returns Exp,
  -- doing the following should be equivalent
  qAddDependentFile path
  contract <- readFile path

  case readContract @cp @st path contract of
    Left e ->
      -- Emit a compiler error if the contract cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the contract.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the contract
        -- can be parsed+typechecked.
        either (error . pretty) snd $
          readContract path contract
      |]
