-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE ApplicativeDo #-}
module Main
  ( main
  ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePretty)
import Fmt (pretty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import Paths_baseDAO_ligo_meta (version)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Util.Main

import Ligo.BaseDAO.Types (Parameter)
import Ligo.Typescript
import Util.CLI
import Util.Named
import Ligo.BaseDAO.TZIP16Metadata

main :: IO ()
main = wrapMain $ do
  cmdLnArgs <- Opt.execParser programInfo
  case cmdLnArgs of
    GenerateTypescript fp ->
      void $ generateTs @Parameter fp
    PrintMetadata mc ->
      putTextLn . decodeUtf8 . encodePretty $
        knownBaseDAOMetadata (mkMetadataSettings mc)

--------------------------------------------------------------------------------
-- Arguments parsing
--------------------------------------------------------------------------------

data CmdArgs
  = PrintMetadata MetadataConfig
  | GenerateTypescript FilePath

cmdArgsParser :: Opt.Parser CmdArgs
cmdArgsParser = asum
  [ Opt.hsubparser $
      mkCommandParser "print-metadata"
        (PrintMetadata <$> metadataConfigParser)
        "Print known part of TZIP-16 metadata."
  , Opt.hsubparser $
      mkCommandParser "generate-typescript"
        (GenerateTypescript <$> (mkCLOptionParser Nothing (#name  .! "target") (#help .! "Path to which generated files should be written.")))
        "Generate typescript type to represent the parameter"
  ]

programInfo :: Opt.ParserInfo CmdArgs
programInfo = Opt.info (Opt.helper <*> versionOption <*> cmdArgsParser) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "baseDAO-ligo-meta: Tools for the LIGO version of the contract."
  ]
  where
    versionOption = Opt.infoOption ("baseDAO-ligo-meta-" <> showVersion version)
      (Opt.long "version" <> Opt.help "Show version.")

-- | Parse metadata for token with given name and given default values.
tokenMetadataParser
  :: String
  -> Text
  -> Text
  -> Word16
  -> Opt.Parser FA2.TokenMetadata
tokenMetadataParser prefix defSymbol defName defDecimals = do
  symbol <-
    mkCLOptionParser (Just defSymbol) (#name .! (prefix <> "-token-symbol"))
    (#help .! "Symbol of the token (according to TZIP-12)")
  name <-
    mkCLOptionParser (Just defName) (#name .! (prefix <> "-token-name"))
    (#help .! "Name of the token (according to TZIP-12)")
  decimals <-
    mkCLOptionParser (Just defDecimals) (#name .! (prefix <> "-token-decimals"))
    (#help .! "Decimals field of the token (according to TZIP-12)")
  return $ FA2.mkTokenMetadata name symbol (pretty decimals)

metadataConfigParser :: Opt.Parser MetadataConfig
metadataConfigParser = do
  mcFrozenTokenMetadata <-
    tokenMetadataParser "frozen" "frozen_token" "Frozen Token" 8
  return MetadataConfig{..}
