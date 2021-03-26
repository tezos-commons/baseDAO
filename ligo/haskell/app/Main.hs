-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE ApplicativeDo #-}
module Main
  ( main
  ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import Paths_baseDAO_ligo_meta (version)

import Util.Main

import BaseDAO.CLI
import Ligo.BaseDAO.Types (ParameterL)
import Ligo.BaseDAO.TZIP16Metadata
import Ligo.Typescript
import Lorentz.Contracts.BaseDAO.TZIP16Metadata
import Util.CLI
import Util.Named

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

main :: IO ()
main = wrapMain $ do
  cmdLnArgs <- Opt.execParser programInfo
  case cmdLnArgs of
    GenerateTypescript fp ->
      void $ generateTs @ParameterL fp
    PrintMetadata mc ->
      putTextLn . decodeUtf8 . encodePretty $
        knownBaseDAOMetadata (mkMetadataSettingsL mc)
