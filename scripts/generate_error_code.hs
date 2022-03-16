#!/usr/bin/env stack
-- stack --resolver lts-17.3 script --package string-interpolate --package universum --package text --package text-manipulate

-- SPDX-FileCopyrightText: 2021 Tocqueville Group
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Script for adding new error to ligo and also update the docs.
module GenerateErrorCode where

import Universum
import Prelude ()

import Data.String.Interpolate (i)
import qualified Data.Text as T
import Data.Text.Manipulate (toCamel)

data ErrorArg
  = NoArg
  | ArgString Text
  deriving (Show, Eq)


data ErrorItem = ErrorItem
  { eiLabel :: Text
  , eiCode :: Integer
  , eiDesc :: Text
  , eiArg :: ErrorArg
  }


data ShortErrorItem = Text :? (Text, ErrorArg)
  deriving (Eq)

removedError :: ShortErrorItem
removedError = "" :? ("", NoArg)

errorsEnumerate :: Integer -> [ShortErrorItem] -> [ErrorItem]
errorsEnumerate start =
  map (\(eiCode, eiLabel :? (eiDesc, eiArg)) -> ErrorItem{..}) .
  filter ((/= removedError) . snd) .
  zip [start..]

invalidInputErrors :: [ErrorItem]
invalidInputErrors = errorsEnumerate 100
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "not_admin"
    :? ("The sender is not the administrator.", NoArg)
  , "not_pending_admin"
    :? ("The sender is not the current pending administrator.", NoArg)
  , "fail_proposal_check"
    :? ("Thrown paired with a `string` error message when the proposal does not pass the `proposal_check`.", ArgString "<reason>")
  , "proposal_not_exist"
    :? ("The proposal does not exist or is no longer ongoing.", NoArg)
  , "voting_stage_over"
    :? ("The proposal voting stage has already ended.", NoArg)
  , removedError
  , removedError
  , "forbidden_xtz"
    :? ("Transfer of XTZ is forbidden on this entrypoint.", NoArg)
  , "proposal_not_unique"
    :? ("The submitted proposal already exist.", NoArg)
  , "missigned"
    :? ("Parameter signature does not match the expected one - for permits.", NoArg)
  , "unpacking_failed"
    :? ("The unpacking of a submitted value failed.", ArgString "<reason>")
  , "unpacking_proposal_metadata_failed"
    :? ("The unpacking of a proposal metadata failed.", NoArg)
  , "missing_value"
    :? ("A required field value was not found.", ArgString "<name of the field>")
  , "not_proposing_stage"
    :? ("Proposals cannot be submitted in non-proposing stages.", NoArg)
  , "not_enough_frozen_tokens"
    :? ("There aren't enough frozen tokens for the operation.", NoArg)
  , "bad_token_contract"
    :? ("The governance token contract does not have the expected entrypoints.", NoArg)
  , "bad_view_contract"
    :? ("The type of a contract for a view entrypoint is not of the expected type.", NoArg)
  , "drop_proposal_condition_not_met"
    :? ("The conditions to drop this proposal are not met.", NoArg)
  , "expired_proposal"
    :? ("The proposal has expired and can no longer be flushed.", NoArg)
  , "empty_flush"
    :? ("There are no available proposals to flush.", NoArg)
  , "not_delegate"
    :? ("The sender has not been delegated the control of the required tokens.", NoArg)
  , "fail_decision_callback"
    :? ("Executing the proposal's decision callback results in failure.", NoArg)
  , removedError
  , "unstake_invalid_proposal"
    :? ("Cannot call `unstake_vote` on the proposal that is not flushed or dropped.", NoArg)
  , "voter_does_not_exist"
    :? ("The sender did not vote on the proposal or already unstaked tokens from the proposal.", NoArg)
  ]

invalidConfigErrors :: [ErrorItem]
invalidConfigErrors = errorsEnumerate 200
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  []

internalErrors :: [ErrorItem]
internalErrors = errorsEnumerate 300
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "bad_state"
    :? ("Throw when storage is in an unexpected state, indicating a contract error.", NoArg)
  ]


---------------------------------------------------------------------------------
-- Ligo
---------------------------------------------------------------------------------

errorsToLigoText :: Text -> [ErrorItem] -> Text
errorsToLigoText title errors = if (length errors /= 0) then
  [i| // ---------------------------------------------------------------------------
// -- #{title}
// ---------------------------------------------------------------------------

#{unlines $ errorItemToLigoText <$> errors}
|] else ""

errorItemToLigoText :: ErrorItem -> Text
errorItemToLigoText ErrorItem{..} =
  [i|(* #{eiDesc} *)
[@inline] let #{eiLabel} = #{eiCode}n
|]

ligoErrorsTemplate :: Text
ligoErrorsTemplate =
  [i|// SPDX-FileCopyrightText: 2021 Tezos Commons
// SPDX-License-Identifier: LicenseRef-MIT-TC

// NOTE: This file should not be modified directly.
// Use `stack scripts/generate_error_code.hs` instead.

\#if ERRORS_CODE_MLIGO
\#else
\#define ERRORS_CODE_MLIGO

\#include "types.mligo"

#{errorsToLigoText "Invalid Input Error Codes" invalidInputErrors}

#{errorsToLigoText "Contract Configuration Error Codes" invalidConfigErrors}

#{errorsToLigoText "Internal Error Codes" internalErrors}

\#endif
|]

---------------------------------------------------------------------------------
-- Markdown
---------------------------------------------------------------------------------

errorItemToMdText :: ErrorItem -> Text
errorItemToMdText ErrorItem{..} =
  "| " <> (show @Text eiCode) <> " | `" <> eiLabel <> "` | " <> eiDesc <> " |"

errorsToMdText :: Text -> [ErrorItem] -> Text
errorsToMdText title errors = if (length errors /= 0) then
  [i| \#\#\#\# #{title}

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
#{unlines $ errorItemToMdText <$> errors}
|] else ""

mdErrorsTemplate :: Text
mdErrorsTemplate =
  [i|<!--
- SPDX-FileCopyrightText: 2021 TQ Tezos

- SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

<!--
NOTE: This file should not be modified directly.
Use `stack scripts/generate_error_code.hs` instead.
-->

\#\# Error Codes

Here is a summary of all the error codes thrown by the contract.


#{errorsToMdText "Invalid Input Error Codes" invalidInputErrors}

#{errorsToMdText "Contract Configuration Error Codes" invalidConfigErrors}

#{errorsToMdText "Internal Error Codes" internalErrors}

|]

---------------------------------------------------------------------------------
-- Haskell
---------------------------------------------------------------------------------

haskellFnName :: ErrorItem -> Text
haskellFnName = toCamel . eiLabel

errorsToHaskellText :: Text -> [ErrorItem] -> Text
errorsToHaskellText title errors = if (length errors /= 0) then
  [i|----------------------------------------------------------------------------
-- #{title}
----------------------------------------------------------------------------
#{unlines $ errorItemToHaskellText <$> errors}

|] else ""

errorItemToHaskellText :: ErrorItem -> Text
errorItemToHaskellText err@ErrorItem{eiDesc, eiCode} =
  [i|
-- | #{eiDesc}
#{fnName} :: Natural
#{fnName} = #{eiCode}|]
  where
    fnName = haskellFnName err

mkMText :: Text -> Text
mkMText a = "[mt|" <> a <>"|]"

errorItemToTzipError :: ErrorItem -> Text
errorItemToTzipError err@ErrorItem{eiDesc, eiCode, eiArg} =
  [i|
  EStatic $ StaticError
    { seError = #{error}
    , seExpansion = toExpression $ toVal @MText #{mkMText eiDesc}
    , seLanguages = ["en"]
    }|]
  where
    fnName = haskellFnName err
    (error :: Text) = case eiArg of
      NoArg ->
        [i|toExpression $ toVal @Integer $ toInteger #{fnName}|]
      ArgString val ->
        [i|toExpression $ toVal @(Integer, MText) (toInteger #{fnName}, #{mkMText val})|]


haskellErrorsTemplate :: Text
haskellErrorsTemplate =
  [i|-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

-- NOTE: This file should not be modified directly.
-- Use @stack scripts/generate_error_code.hs@ instead.

{-\# OPTIONS_GHC -Wno-missing-export-lists \#-}

module Ligo.BaseDAO.ErrorCodes where

import Universum

import Morley.Micheline (toExpression)
import Morley.Michelson.Text
import Morley.Michelson.Typed (toVal)
import Lorentz.Contracts.Spec.TZIP16Interface

#{errorsToHaskellText "Invalid Input Error Codes" invalidInputErrors}

#{errorsToHaskellText "Contract Configuration Error Codes" invalidConfigErrors}

#{errorsToHaskellText "Internal Error Codes" internalErrors}

----------------------------------------------------------------------------
-- TZIP 16 error list
----------------------------------------------------------------------------
tzipErrorList :: [Error]
tzipErrorList = [#{ T.intercalate "\n  ," $ errorItemToTzipError <$> (invalidInputErrors <> invalidConfigErrors <> internalErrors)}
  ]

|]


main :: IO ()
main = do
  writeFile "src/error_codes.mligo" ligoErrorsTemplate
  writeFile "docs/error-codes.md" mdErrorsTemplate
  writeFile "haskell/src/Ligo/BaseDAO/ErrorCodes.hs" haskellErrorsTemplate
