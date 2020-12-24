-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoRebindableSyntax #-}

module BaseDAO.CLI
  ( daoStorageParser
  , daoStorageParserDef
  , DaoContractInfo (..)
  , daoContractRegistry

  , serveContractRegistry
  , contractRegistryProgramInfo
  , contractRegistryUsageDoc
  ) where

import Universum

import qualified Data.Map as Map
import Data.Version (Version, showVersion)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz.ContractRegistry
import Lorentz.Doc
import Lorentz.Value
import Morley.CLI (addressOption)
import Util.CLI
import Util.Named

import qualified Lorentz.Contracts.BaseDAO as DAO

------------------------------------------------------------------------
-- DAO
------------------------------------------------------------------------

daoStorageParser :: Opt.Parser ce -> Opt.Parser (DAO.Storage ce pm)
daoStorageParser extraParser = do
  adminAddress <-
    addressOption Nothing (#name .! "admin")
    (#help .! "Administrator of the DAO contract")
  votingPeriod <-
    mkCLOptionParser (Just $ 60 * 60 * 24 * 7) (#name .! "voting-period")
    (#help .! "Period after which proposals can be finished")
  quorumThreshold <-
    mkCLOptionParser (Just 4) (#name .! "quorum-threshold")
    (#help .! "Total number of votes necessary for successful proposal")
  extra <-
    extraParser
  pure $ DAO.mkStorage
    (#admin .! adminAddress)
    (#votingPeriod .? Just votingPeriod)
    (#quorumThreshold .? Just quorumThreshold)
    (#extra .! extra)

daoStorageParserDef :: Default ce => Opt.Parser (DAO.Storage ce pm)
daoStorageParserDef = daoStorageParser (pure def)

data DaoContractInfo =
  forall ce pm op. DAO.DaoC ce pm op =>
  DaoContractInfo
  { dciName :: Text
  , dciConfig :: DAO.Config ce pm op
  , dciExtraParser :: Opt.Parser ce
  }

daoContractRegistry :: [DaoContractInfo] -> ContractRegistry
daoContractRegistry contracts = ContractRegistry . Map.fromList $
  contracts <&> \DaoContractInfo{..} ->
    dciName ?:: ContractInfo
      { ciContract = DAO.baseDaoContract dciConfig
      , ciIsDocumented = True
      , ciStorageParser = Just $ daoStorageParser dciExtraParser
      , ciStorageNotes = Nothing
      }

------------------------------------------------------------------------
-- Contract registry
------------------------------------------------------------------------

-- | Put this to @main@ to make the executable serve given contracts.
serveContractRegistry :: String -> DGitRevision -> ContractRegistry -> Version -> IO ()
serveContractRegistry execName gitRev contracts version =
  withUtf8 $ do
    cmdLnArgs <-
      Opt.execParser $
      contractRegistryProgramInfo version gitRev execName contracts
    runContractRegistry contracts cmdLnArgs `catchAny` (die . displayException)

contractRegistryProgramInfo
  :: Version
  -> DGitRevision
  -> String
  -> ContractRegistry
  -> Opt.ParserInfo CmdLnArgs
contractRegistryProgramInfo version gitRev execName registry =
  Opt.info (Opt.helper <*> versionOption <*> argParser registry gitRev) $
    mconcat
    [ Opt.fullDesc
    , Opt.progDesc $ execName <> " contracts registry"
    , Opt.header $ execName <> " contracts for Michelson"
    , Opt.footerDoc $ contractRegistryUsageDoc execName
    ]
  where
    versionOption = Opt.infoOption (execName <> "-" <> showVersion version)
      (Opt.long "version" <> Opt.help "Show version.")

contractRegistryUsageDoc :: String -> Maybe Doc
contractRegistryUsageDoc execName = Just $ mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  " <> fromString execName <> " print --help", linebreak
   ]
