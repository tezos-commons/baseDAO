-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Read a contract at compile time.
module Ligo.Util
  ( fetchContract
  , fetchValue
  ) where

import Universum

import Fmt (pretty)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import qualified Language.Haskell.TH.Syntax as TH
import System.Environment (lookupEnv)

import Michelson.Parser
import Michelson.Test.Import
import Michelson.TypeCheck.Instr
import Michelson.Typed

-- | Read a contract at compile time assuming its expected type is known.
--
-- This is not an ideal implementation, e.g. it does not pretty-print
-- types in error messages on types mismatch.
fetchContract :: forall cp st. (KnownT cp, KnownT st) => String -> TH.ExpQ
fetchContract envKey = do
  (path, contract) <- readDependentSource "ligo/haskell/test/baseDAO.tz"  envKey
                          -- ↑ This default path works on CI.
                          -- There it's relative to the repo root, apparently.

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


-- | Reads a Michelson expression into a known typed value at compile type.
fetchValue :: forall st. KnownT (ToT st) => FilePath -> String -> TH.ExpQ
fetchValue defaultPath envKey = do
  valueLiteral <- snd <$> readDependentSource defaultPath envKey
  case readTypedValue_ @(ToT st) valueLiteral of
    Left e ->
      -- Emit a compiler error if the value cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the value.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the contract
        -- can be parsed+typechecked.
        either (error . pretty) id $
          readTypedValue_ valueLiteral
      |]

readDependentSource
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> String
  -> m (FilePath, Text)
readDependentSource defaultPath envKey = do
  path <- fromMaybe defaultPath <$> liftIO (lookupEnv envKey)
  -- We cannot use 'embedFile' directly because it returns Exp,
  -- doing the following should be equivalent
  qAddDependentFile path
  contents <- readFile path
  pure (path, contents)

readTypedValue_ :: forall st. (KnownT st) => Text -> Either Text (Value st)
readTypedValue_ valueLiteral = do
  uValue <- first pretty $ parseExpandValue valueLiteral
  first pretty $ typeVerifyStorage @st uValue
