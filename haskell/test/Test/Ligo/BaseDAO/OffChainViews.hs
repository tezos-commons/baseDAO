-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import qualified Data.Map as M
import Control.Lens ((&~), (.=))
import Fmt (pretty)

import Lorentz.Value
import Michelson.Interpret (MichelsonFailed)
import Michelson.Test.Dummy
import Morley.Metadata
import Named (defaults, (!))
import Test.HUnit (assertBool, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Tezos.Address
import Util.Named ((.!))

import Test.Ligo.BaseDAO.Common (totalSupplyFromLedger)
import Ligo.BaseDAO.Types
import Ligo.BaseDAO.TZIP16Metadata
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface

offChainViewStorage :: Storage
offChainViewStorage =
  (mkStorage
  ! #admin addr
  ! #extra dynRecUnsafe
  ! #metadata mempty
  ! #now (timestampFromSeconds 0)
  ! defaults
  ) { sLedger = bal
    , sTotalSupply = totalSupplyFromLedger bal
    }
  where
    addr = unsafeParseAddress "tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE"
    bal = BigMap $ M.fromList $
      [ ((addr, unfrozenTokenId), 100)
      , ((addr, frozenTokenId), 200)
      ]

test_FA2 :: TestTree
test_FA2 =
  mkFA2Tests offChainViewStorage (mkMetadataSettings defaultMetadataConfig)

runView
  :: forall ret. IsoValue ret
  => View $ ToT Storage
  -> Storage
  -> ViewParam
  -> Either MichelsonFailed ret
runView view storage param =
  case interpretView dummyContractEnv view param storage of
    Right x -> Right x
    Left (VIEMichelson _ (MSVIEMichelsonFailed e)) -> Left e
    Left err -> error (pretty err)

addr1, addr2 :: Address
addr1 = unsafeParseAddress "tz1Sm4RZtPho7FDMHw2Bnszduu8FVFCeczpm"
addr2 = unsafeParseAddress "tz1R2kv7Uzr8mgeJYEgi7KGQiHFbCg3WN6YC"

mkFA2Tests :: Storage -> MetadataSettings -> TestTree
mkFA2Tests storage mc = testGroup "FA2 off-chain views"
  [ testGroup "is_operator" $
    let checkOperator st param =
          runView @Bool (isOperatorView mc) st (ViewParam param)
    in
    [ testCase "Empty storage" $
        checkOperator
          storage
          FA2.OperatorParam
            { opOwner = addr1, opOperator = addr2, opTokenId = FA2.theTokenId }
        @?= Right False

    , testCase "Present operator" $
        let
          store = storage &~
            mcOperatorsL mc .= BigMap (one ((#owner .! addr1, #operator .! addr2), ()))
        in
          checkOperator store FA2.OperatorParam
            { opOwner = addr1, opOperator = addr2, opTokenId = FA2.theTokenId }
          @?= Right True

    , testCase "Invalid token_id" $
        assertBool "Unexpectedly succeeded" . isLeft $
        checkOperator
          storage
          FA2.OperatorParam
            { opOwner = addr1, opOperator = addr2, opTokenId = FA2.TokenId 5 }
    ]

  , testGroup "token_metadata" $
    [ testCase "Get frozen tokens metadata" $
        runView @(FA2.TokenId, FA2.TokenMetadata)
          (tokenMetadataView mc) storage (ViewParam frozenTokenId)
        @?= Right (frozenTokenId, mcFrozenTokenMetadata defaultMetadataConfig)
    ]
  , testGroup "get_total_supply" $
    let checkTotalSupply st param =
          runView @Natural (getTotalSupplyView mc) st (ViewParam param)
    in
    [ testCase "Get unfrozen token total supply" $
        checkTotalSupply storage unfrozenTokenId
          @?= Right 100
    , testCase "Get frozen token total supply" $
        checkTotalSupply storage frozenTokenId
          @?= Right 200
    ]
  ]
