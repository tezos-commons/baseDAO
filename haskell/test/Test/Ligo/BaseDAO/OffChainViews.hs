-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import Fmt (pretty)

import Lorentz.Value
import Michelson.Interpret (MichelsonFailed)
import Michelson.Runtime.GState (genesisAddress)
import Michelson.Test.Dummy
import Morley.Metadata
import Named (defaults, (!))
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Tezos.Address

import Ligo.BaseDAO.TZIP16Metadata
import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface

offChainViewStorage :: Storage
offChainViewStorage =
  (mkStorage
  ! #admin addr
  ! #extra dynRecUnsafe
  ! #metadata mempty
  ! #level 100
  ! #tokenAddress genesisAddress
  ! #quorumThreshold (mkQuorumThreshold 1 100)
  ! defaults
  )
  where
    addr = unsafeParseAddress "tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE"

test_FA2 :: TestTree
test_FA2 =
  mkFA2Tests offChainViewStorage (mkMetadataSettings defaultMetadataConfig)

runView
  :: forall ret. (HasCallStack, IsoValue ret)
  => View $ ToT Storage
  -> Storage
  -> ViewParam
  -> Either MichelsonFailed ret
runView view storage param =
  case interpretView dummyContractEnv view param storage of
    Right x -> Right x
    Left (VIEMichelson _ (MSVIEMichelsonFailed e)) -> Left e
    Left err -> error (pretty err)


mkFA2Tests :: Storage -> MetadataSettings -> TestTree
mkFA2Tests storage mc = testGroup "FA2 off-chain views"
  [ testGroup "governance_token" $
    [ testCase "Get the address and token_id of the associated FA2 contract" $
        runView @GovernanceToken (governanceTokenView mc) storage NoParam
        @?= (Right $ GovernanceToken genesisAddress FA2.theTokenId)
    ]
  ]
