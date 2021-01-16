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
import Ligo.BaseDAO.TZIP16Metadata
import Lorentz.Contracts.BaseDAO.TZIP16Metadata

data CmdArgs
  = PrintMetadata

cmdArgsParser :: Opt.Parser CmdArgs
cmdArgsParser = asum
  [ Opt.hsubparser $
      mkCommandParser "print-metadata" (pure PrintMetadata)
        "Print known part of TZIP-16 metadata."
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
    PrintMetadata ->
      putTextLn . decodeUtf8 . encodePretty $
        knownBaseDAOMetadata defaultMetadataConfigL
