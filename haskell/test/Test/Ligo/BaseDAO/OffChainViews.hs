-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE OverloadedLists #-}

module Test.Ligo.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BS

import Morley.Michelson.Runtime.GState (genesisAddress)
import Morley.Tezos.Address
import Named (defaults, (!))
import Test.Cleveland
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Ligo.BaseDAO.TZIP16Metadata
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap, metadataURI, selfHost, tezosStorageUri)
import Test.Morley.Metadata

metadataBigMap :: MetadataConfig -> MetadataMap
metadataBigMap conf = metadataURI (tezosStorageUri selfHost "metadata")
  <> [("metadata", BS.toStrict $ encode $ knownBaseDAOMetadata $ mkMetadataSettings conf)]

offChainViewStorage :: Storage
offChainViewStorage =
  (mkStorage
  ! #admin addr
  ! #extra ()
  ! #metadata (metadataBigMap defaultMetadataConfig)
  ! #level 100
  ! #tokenAddress (toAddress genesisAddress)
  ! #quorumThreshold (mkQuorumThreshold 1 100)
  ! #config defaultConfig
  ! defaults
  )
  where
    addr = [ta|tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE|]

test_FA2 :: TestTree
test_FA2 = testGroup "FA2 off-chain views"
  [ testGroup "governance_token" $
    [ testScenario "can flush proposals that got accepted" $ scenario $ do
          baseDao <- originate "BaseDAO - Test Contract" offChainViewStorage baseDAOContractLigo
          callOffChainView @GovernanceToken baseDao "governance_token" NoParam
            @@== (GovernanceToken (toAddress genesisAddress) FA2.theTokenId)
    ]
  ]
