-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE ApplicativeDo #-}
module Main
  ( main
  ) where

import Universum

import Lorentz (Default(..), GitRepoSettings(..), mkDGitRevision)
import Lorentz.ContractRegistry
import Paths_baseDAO (version)

import BaseDAO.CLI
import qualified Lorentz.Contracts.GameDAO as GameDAO
import qualified Lorentz.Contracts.TrivialDAO as TrivialDAO

repoSettings :: GitRepoSettings
repoSettings = GitRepoSettings $ \commit ->
  "https://github.com/tqtezos/baseDAO/tree/" <> commit

contracts :: ContractRegistry
contracts = daoContractRegistry
  [ DaoContractInfo
    { dciName = "TrivialDAO"
    , dciConfig = TrivialDAO.trivialConfig
    , dciExtraParser = pure def
    }
  , DaoContractInfo
    { dciName = "GameDAO"
    , dciConfig = GameDAO.config
    , dciExtraParser = pure def
    }
  ]

main :: IO ()
main = serveContractRegistry "BaseDAO" ($mkDGitRevision repoSettings) contracts version
