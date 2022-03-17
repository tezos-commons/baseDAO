-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE ApplicativeDo #-}
module Main
  ( main
  ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import Paths_baseDAO_ligo_meta (version)

import Morley.Util.Main

import Ligo.BaseDAO.Types (Parameter, Parameter')
import Ligo.BaseDAO.RegistryDAO.Types (RegistryCustomEpParam)
import Ligo.BaseDAO.TreasuryDAO.Types (TreasuryCustomEpParam)
import Ligo.Typescript
import Morley.Util.CLI
import Morley.Util.Named
import Ligo.BaseDAO.TZIP16Metadata

main :: IO ()
main = wrapMain $ do
  cmdLnArgs <- Opt.execParser programInfo
  case cmdLnArgs of
    GenerateTypescript fp -> do
      void $ generateTs @Parameter fp
      void $ generateTs @(Parameter' RegistryCustomEpParam) fp
      void $ generateTs @(Parameter' TreasuryCustomEpParam) fp
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
        (GenerateTypescript <$> (mkCLOptionParser Nothing (#name  :! "target") (#help :! "Path to which generated files should be written.")))
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
  -> Text
  -> Opt.Parser MetadataConfig
tokenMetadataParser prefix defSymbol defName defThumbnailUri = do
  symbol <-
    mkCLOptionParser (Just defSymbol) (#name :! (prefix <> "-token-symbol"))
    (#help :! "Symbol of the token (according to TZIP-12)")
  name <-
    mkCLOptionParser (Just defName) (#name :! (prefix <> "-token-name"))
    (#help :! "Name of the token (according to TZIP-12)")
  thumbnailUri <-
    mkCLOptionParser (Just defThumbnailUri) (#name :! (prefix <> "-token-thumbnail-uri"))
    (#help :! "Thumbnail URI of the token (according to TZIP-21)")

  return $ MetadataConfig name symbol 0 (if thumbnailUri == "" then Nothing else Just thumbnailUri)

metadataConfigParser :: Opt.Parser MetadataConfig
metadataConfigParser =
  tokenMetadataParser "frozen" "frozen_token" "BaseDAO Frozen Token" ""
