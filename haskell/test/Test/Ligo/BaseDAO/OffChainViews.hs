-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import Morley.Michelson.Runtime.GState (genesisAddress)
import Morley.Tezos.Address
import Named (defaults, (!))
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Types
import Ligo.BaseDAO.TZIP16Metadata

offChainViewStorage :: Storage
offChainViewStorage =
  (mkStorage
  ! #admin addr
  ! #extra ()
  ! #metadata mempty
  ! #level 100
  ! #tokenAddress (MkAddress genesisAddress)
  ! #quorumThreshold (mkQuorumThreshold 1 100)
  ! #config defaultConfig
  ! defaults
  )
  where
    addr = [ta|tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE|]

test_FA2 :: TestTree
test_FA2 =
  mkFA2Tests offChainViewStorage (mkMetadataSettings defaultMetadataConfig)

-- runView
--   :: forall ret mp. (HasCallStack, IsoValue ret)
--   => View $ ToT Storage
--   -> Storage
--   -> ViewParam mp
--   -> Either MichelsonFailed ret
-- runView view storage param =
--   case interpretView dummyContractEnv view param storage of
--     Right x -> Right x
--     Left (VIEMichelson _ (MSVIEMichelsonFailed (MichelsonFailureWithStack e _))) -> Left e
--     Left err -> error (pretty err)

mkFA2Tests :: Storage -> MetadataSettings -> TestTree
mkFA2Tests _ _ = testGroup "FA2 off-chain views" []
-- TODO: Uncomment when some way to run views is available.
--   [ testGroup "governance_token" $
--     [ testCase "Get the address and token_id of the associated FA2 contract" $
--         runView @GovernanceToken (governanceTokenView mc) storage NoParam
--         @?= (Right $ GovernanceToken (MkAddress genesisAddress) FA2.theTokenId)
--     ]
--   ]
