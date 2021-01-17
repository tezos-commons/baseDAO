-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module BaseDAO.ShareTest.OffChainViews
  ( mkFA2Tests
  ) where

import Lorentz.Value
import Universum hiding (view)

import Control.Lens ((&~), (.=))
import Fmt (pretty)
import Test.HUnit (assertBool, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface
import Michelson.Interpret (MichelsonFailed)
import Michelson.Test.Dummy
import Morley.Metadata
import Tezos.Address
import Util.Named ((.!))

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.BaseDAO.TZIP16Metadata as DAO

runView
  :: forall ret store.
    (IsoValue ret, IsoValue store)
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
  => store -> DAO.MetadataConfig store -> TestTree
mkFA2Tests defaultStorage mc = testGroup "FA2 off-chain views"
  [ testGroup "is_operator" $
    let checkOperator st param =
          runView @Bool (DAO.isOperatorView mc) st (ViewParam param)
    in
    [ testCase "Empty storage" $
        checkOperator
          defaultStorage
          FA2.OperatorParam
            { opOwner = addr1, opOperator = addr2, opTokenId = FA2.theTokenId }
        @?= Right False

    , testCase "Present operator" $
        let
          store = defaultStorage &~
            DAO.mcOperatorsL mc .= BigMap (one ((#owner .! addr1, #operator .! addr2), ()))
        in
          checkOperator store FA2.OperatorParam
            { opOwner = addr1, opOperator = addr2, opTokenId = FA2.theTokenId }
          @?= Right True

    , testCase "Invalid token_id" $
        assertBool "Unexpectedly succeeded" . isLeft $
        checkOperator
          defaultStorage
          FA2.OperatorParam
            { opOwner = addr1, opOperator = addr2, opTokenId = FA2.TokenId 5 }
    ]

  , testGroup "token_metadata" $
    [ testCase "Get frozen tokens metadata" $
        runView @(FA2.TokenId, FA2.TokenMetadata)
          (DAO.tokenMetadataView mc) defaultStorage (ViewParam DAO.frozenTokenId)
        @?= Right (DAO.frozenTokenId, DAO.mcFrozenTokenMetadata DAO.defaultMetadataConfig)
    ]
  ]
