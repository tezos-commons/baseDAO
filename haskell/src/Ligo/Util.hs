-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Read a contract at compile time.
module Ligo.Util
  ( fetchValue
  ) where

import Universum

import Data.FileEmbed (makeRelativeToProject)
import qualified Data.Text.IO.Utf8 as Utf8 (readFile)
import Fmt (pretty)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (qAddDependentFile)

import Morley.Michelson.Runtime.Import (MichelsonSource(..), readValue)
import Morley.Michelson.Typed

-- TODO morley/659 replace this with embedValue
--
-- | Reads a Michelson expression into a known typed value at compile type.
fetchValue :: forall st. KnownIsoT st => FilePath -> ExpQ
fetchValue rawPath = do
  -- We cannot use 'embedFile' directly because it returns Exp,
  -- doing the following should be equivalent
  path <- makeRelativeToProject rawPath
  qAddDependentFile path
  valueLiteral <- Utf8.readFile path

  case readValue @(ToT st) (MSFile path) valueLiteral of
    Left e ->
      -- Emit a compiler error if the value cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the value.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the
        -- value can be parsed+typechecked.
        either (error . pretty) fromVal $ readValue "" valueLiteral
      |]
