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

    -- * Helpers
  , mkCommandParser
  ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map as Map
import Data.Version (Version, showVersion)
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz.ContractRegistry
import Lorentz.Contracts.MetadataCarrier (metadataCarrierContract)
import Lorentz.Doc
import Lorentz.Value
import Morley.CLI (addressOption)
import Util.CLI
import Util.Main (wrapMain)
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
  metadata <-
    tzip16MetadataParser
  pure $ DAO.mkStorage
    (#admin .! adminAddress)
    (#votingPeriod .? Just votingPeriod)
    (#quorumThreshold .? Just quorumThreshold)
    (#extra .! extra)
    (#metadata .! metadata)

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
daoContractRegistry contracts = ContractRegistry $ mconcat
  [ Map.fromList $ contracts <&> \DaoContractInfo{..} ->
    dciName ?:: ContractInfo
      { ciContract = DAO.baseDaoContract dciConfig
      , ciIsDocumented = True
      , ciStorageParser = Just $ daoStorageParser dciExtraParser
      , ciStorageNotes = Nothing
      }

  , one $ "MetadataCarrier" ?:: ContractInfo
      { ciContract = metadataCarrierContract
      , ciIsDocumented = False
      , ciStorageParser = Nothing
      , ciStorageNotes = Nothing
      }
  ]

------------------------------------------------------------------------
-- TZIP-16 metadata
------------------------------------------------------------------------

tzip16MetadataParser :: Opt.Parser (TZIP16.MetadataMap BigMap)
tzip16MetadataParser = do
  -- The primary scenario we support - reference to a contract
  -- with actual metadata
  contract <- contractHostParser
  key <- metadataKeyParser
  return $ TZIP16.metadataURI (TZIP16.tezosStorageUri contract key)
  where
    contractHostParser :: Opt.Parser TZIP16.ContractHost
    contractHostParser = do
      address <-
        mkCLOptionParser Nothing (#name .! "metadata-host-address")
        (#help .! "Address of the contract storing actual metadata")
      mChainId <- Opt.optional . fmap fromString $
        mkCLOptionParser Nothing (#name .! "metadata-host-chain")
        (#help .! "Chain where the contract storing actual metadata resides")
      return $
        maybe TZIP16.contractHost TZIP16.foreignContractHost mChainId address

    metadataKeyParser :: Opt.Parser MText
    metadataKeyParser =
      mkCLOptionParser Nothing (#name .! "metadata-key")
        (#help .! "Key in metadata host contract where baseDAO's metadata is located")

------------------------------------------------------------------------
-- Contract registry
------------------------------------------------------------------------

data AllCmdLnArgs
  = ContractRegistryCmd CmdLnArgs
  | PrintMetadata

allArgsParser :: DGitRevision -> ContractRegistry -> Opt.Parser AllCmdLnArgs
allArgsParser gitRev registry = asum
  [ ContractRegistryCmd <$>
      argParser registry gitRev

  , Opt.hsubparser $
      mkCommandParser "print-metadata" (pure PrintMetadata)
        "Print known part of TZIP-16 metadata."
  ]

mkCommandParser
  :: String
  -> Opt.Parser a
  -> String
  -> Opt.Mod Opt.CommandFields a
mkCommandParser commandName parser desc =
  Opt.command commandName $ Opt.info parser $ Opt.progDesc desc

-- | Put this to @main@ to make the executable serve given contracts.
serveContractRegistry :: String -> DGitRevision -> ContractRegistry -> Version -> IO ()
serveContractRegistry execName gitRev contracts version =
  wrapMain $ do
    allCmdLnArgs <-
      Opt.execParser $
      contractRegistryProgramInfo version gitRev execName contracts
    case allCmdLnArgs of
      ContractRegistryCmd cmdLnArgs ->
        runContractRegistry contracts cmdLnArgs
      PrintMetadata ->
        putTextLn . decodeUtf8 . encodePretty $
          DAO.knownBaseDAOMetadata DAO.defaultMetadataConfig

contractRegistryProgramInfo
  :: Version
  -> DGitRevision
  -> String
  -> ContractRegistry
  -> Opt.ParserInfo AllCmdLnArgs
contractRegistryProgramInfo version gitRev execName registry =
  Opt.info (Opt.helper <*> versionOption <*> allArgsParser gitRev registry) $
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
