-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.OffChainViews
  ( test_FA2
  ) where

import Universum hiding (view)

import Control.Lens ((&~), (.=))
import qualified Data.Map as M
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

import qualified Lorentz.Contracts.BaseDAO.TZIP16Metadata as DAO
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface
import Test.BaseDAO.Common (totalSupplyFromLedger)

offChainViewStorage :: DAO.Storage () ()
offChainViewStorage =
  (DAO.mkStorage
  ! #admin addr
  ! #extra ()
  ! #metadata mempty
  ! defaults
  ) { DAO.sLedger = bal
    , DAO.sTotalSupply = totalSupplyFromLedger bal
    }
  where
    addr = unsafeParseAddress "tz1M6dcor9QNTFr9Ri68cBYvpxrogZaMttuE"
    bal = BigMap $ M.fromList $
      [ ((addr, DAO.unfrozenTokenId), 100)
      , ((addr, DAO.frozenTokenId), 200)
      ]

test_FA2 :: TestTree
test_FA2 =
  mkFA2Tests offChainViewStorage (DAO.mkMetadataSettings DAO.defaultMetadataConfig)


runView
  :: forall ret store.
    (HasCallStack, IsoValue ret, IsoValue store)
  => View $ ToT store
  -> store
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

mkFA2Tests
  :: (IsoValue store)
  => store -> DAO.MetadataSettings store -> TestTree
mkFA2Tests storage mc = testGroup "FA2 off-chain views"
  [ testGroup "is_operator" $
    let checkOperator st param =
          runView @Bool (DAO.isOperatorView mc) st (ViewParam param)
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
            DAO.mcOperatorsL mc .= BigMap (one ((#owner .! addr1, #operator .! addr2), ()))
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
          (DAO.tokenMetadataView mc) storage (ViewParam DAO.frozenTokenId)
        @?= Right (DAO.frozenTokenId, DAO.mcFrozenTokenMetadata DAO.defaultMetadataConfig)
    ]
  , testGroup "get_total_supply" $
    let checkTotalSupply st param =
          runView @Natural (DAO.getTotalSupplyView mc) st (ViewParam param)
    in
    [ testCase "Get unfrozen token total supply" $
        checkTotalSupply storage DAO.unfrozenTokenId
          @?= Right 100
    , testCase "Get frozen token total supply" $
        checkTotalSupply storage DAO.frozenTokenId
          @?= Right 200
    ]
  ]
