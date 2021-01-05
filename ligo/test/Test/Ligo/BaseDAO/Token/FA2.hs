-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Token.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import Lorentz
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenario)
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Lorentz.Contracts.BaseDAO.Types

import qualified Ligo.BaseDAO.Contract as Ligo
import qualified Ligo.BaseDAO.Types as Ligo
import qualified Ligo.BaseDAO.Helper as Ligo
import Michelson.Typed.Convert (convertContract, untypeValue)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified BaseDAO.ShareTest.FA2 as Share

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "BaseDAO FA2 tests:"
  [ testGroup "Operator:"
      [ nettestScenario "allows zero transfer from non-existent operator"
          $ uncapsNettest $ Share.zeroTransferScenario False originateLigoDao
      , nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferScenario False originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenScenario False originateLigoDao
      , nettestScenario "accepts an empty list of transfers"
          $ uncapsNettest $ Share.emptyTransferListScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to non existent source account)"
          $ uncapsNettest $ Share.noSourceAccountScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to bad operator)"
          $ uncapsNettest $ Share.badOperatorScenario False originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyScenario False originateLigoDao
      ]
  , testGroup "Owner:"
      [ nettestScenario "allows valid transfer and check balance"
          $ uncapsNettest $ Share.validTransferOwnerScenario False originateLigoDao
      , nettestScenario "allows updating operator "
          $ uncapsNettest $ Share.updatingOperatorScenario False originateLigoDao
      , nettestScenario "allows balanceOf request"
          $ uncapsNettest $ Share.balanceOfOwnerScenario False originateLigoDao
      , nettestScenario "validates token id"
          $ uncapsNettest $ Share.validateTokenOwnerScenario False originateLigoDao
      , nettestScenario "aborts if there is a failure (due to low balance)"
          $ uncapsNettest $ Share.lowBalanceOwnerScenario False originateLigoDao
      , nettestScenario "cannot transfer foreign money"
          $ uncapsNettest $ Share.noForeignMoneyOwnerScenario False originateLigoDao
      ]
  , testGroup "Admin:"
    [ nettestScenario "transfer tokens from any address to any address"
        $ uncapsNettest $ Share.adminTransferScenario False originateLigoDao
    , nettestScenario "transfer frozen tokens"
        $ uncapsNettest $ Share.adminTransferFrozenScenario False $ originateLigoDaoWithBalance
            (\owner1 owner2 ->
                [ ((owner1, frozenTokenId), 100)
                , ((owner2, frozenTokenId), 100)
                ]
            )
    ]
  -- TODO #100: Migration not implemented
  -- , testGroup "Entrypoints respect migration status"
  --     [ nettestScenario "transfer respects migration status"
  --         $ uncapsNettest $ Share.transferAfterMigrationScenario False originateLigoDao
  --     , nettestScenario "update operator respects migration status "
  --         $ uncapsNettest $ Share.updatingOperatorAfterMigrationScenario False originateLigoDao
  --     , nettestScenario "balanceOf request respects migration status"
  --         $ uncapsNettest $ Share.balanceOfRequestAfterMigrationScenario False originateLigoDao
  --     , nettestScenario "tokenMetadataRegistry request respects migration status"
  --         $ uncapsNettest $ Share.tokenMetadataRegistryRequestAfterMigrationScenario False originateLigoDao
  --     ]
  ]

originateLigoDaoWithBalance
 :: forall caps base m. (MonadNettest caps base m)
 => (Address -> Address -> [(LedgerKey, LedgerValue)])
 -> m ((Address, Address), (Address, Address), TAddress Ligo.Parameter, Address)
originateLigoDaoWithBalance balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let bal = BigMap $ M.fromList $ balFunc owner1 owner2
  let operators = BigMap $ M.fromSet (const ()) $ S.fromList
        [ (#owner .! owner1, #operator .! operator1)
        , (#owner .! owner2, #operator .! operator2)
        ]

  let fullStorage = Ligo.mkFullStorage admin bal operators

  let
    originateData = UntypedOriginateData
      { uodFrom = nettestAddress
      , uodName = "BaseDAO"
      , uodBalance = toMutez 0
      , uodStorage = untypeValue $ toVal $ fullStorage
      , uodContract = convertContract Ligo.baseDAOContractLigo
      }
  daoUntyped <- originateUntyped originateData
  let dao = TAddress @Ligo.Parameter daoUntyped

  pure ((owner1, operator1), (owner2, operator2), dao, admin)

originateLigoDao
 :: forall caps base m. (MonadNettest caps base m)
 => m ((Address, Address), (Address, Address), TAddress Ligo.Parameter, Address)
originateLigoDao =
  originateLigoDaoWithBalance (\owner1_ owner2_ ->
    [ ((owner1_, Ligo.unfrozenTokenId), 100)
    , ((owner2_, Ligo.unfrozenTokenId), 100)
    ])
