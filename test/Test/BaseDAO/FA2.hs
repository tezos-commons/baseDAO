-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.BaseDAO.FA2
  ( test_BaseDAO_FA2
  ) where

import Universum

import qualified Data.Map.Strict as Map
import Lorentz
import Lorentz.Test
import Morley.Nettest
import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util.Named

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.BaseDAO as DAO

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

ignore :: Expectation -> Expectation
ignore _ = pass

originateBaseDAO
  :: MonadNettest caps base m
  => m ((Address, Address), (Address, Address), TAddress FA2.Parameter, Address)
originateBaseDAO = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let bal = Map.fromList
        [ ((owner1, 0), 100)
        , ((owner2, 0), 100)
        ]
  let (operators :: DAO.Operators) = BigMap $ Map.fromList
        [ ((owner1, (operator1, 0)), ())
        , ((owner2, (operator2, 0)), ())
        ]
  let
    originateData = OriginateData
      { odFrom = AddressResolved genesisAddress
      , odName = "BaseDAO"
      , odBalance = toMutez 0
      , odStorage = DAO.mkStorage admin bal operators
      , odContract = DAO.baseDaoContract
      }
  dao <- originate originateData
  pure ((owner1, operator1), (owner2, operator2), dao, admin)

test_BaseDAO_FA2 :: TestTree
test_BaseDAO_FA2 = testGroup "tests to check BaseDAO contract functionality"
  [ testGroup "Operator:"
      [ testCase "allows zero transfer from non-existent operator" $
          nettestTestExpectation zeroTransferScenario
      , testCase "allows valid transfer and check balance" $
          ignore $ nettestTestExpectation validTransferScenario
      , testCase "validates token id" $
          ignore $ nettestTestExpectation validateTokenScenario
      , testCase "accepts an empty list of transfers" $
          nettestTestExpectation emptyTransferListScenario
      , testCase "aborts if there is a failure (due to low balance)" $
          ignore $ nettestTestExpectation lowBalanceScenario
      , testCase "aborts if there is a failure (due to non existent source account)" $
          ignore $ nettestTestExpectation noSourceAccountScenario
      , testCase "aborts if there is a failure (due to bad operator)" $
          ignore $ nettestTestExpectation badOperatorScenario
      , testCase "cannot transfer foreign money" $
          ignore $ nettestTestExpectation noForeignMoneyScenario
      ]
  , testGroup "Owner:"
      [ testCase "allows valid transfer and check balance" $
          ignore $ nettestTestExpectation validTransferOwnerScenario
      , testCase "allows updating operator " $
          ignore $ nettestTestExpectation updatingOperatorScenario
      , testCase "allows balanceOf request" $
          ignore $ nettestTestExpectation balanceOfOwnerScenario
      , testCase "validates token id" $
          ignore $ nettestTestExpectation validateTokenOwnerScenario
      , testCase "aborts if there is a failure (due to low balance)" $
          ignore $ nettestTestExpectation lowBalanceOwnerScenario
      , testCase "cannot transfer foreign money" $
          ignore $ nettestTestExpectation noForeignMoneyOwnerScenario
      ]
  , testGroup "Admin:"
    [ testCase "transfer tokens from any address to any address" $
        ignore $ nettestTestExpectation adminTransferScenario
    ]
  ]

zeroTransferScenario :: (Monad m) => NettestImpl m -> m ()
zeroTransferScenario = uncapsNettest $ do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! nonexistent
          , #txs .! [ (#to_ .! owner1, (#token_id .! 0, #amount .! 0)) ]
          )
        ]
  callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params

validTransferScenario :: (Monad m) => NettestImpl m -> m ()
validTransferScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner1
          , #txs .! [ (#to_ .! owner2, (#token_id .! 0, #amount .! 10)) ]
          )
        ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params

  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved owner2) dao (Call @"Balance_of")
    (mkFA2View [ (#owner .! owner2, #token_id .! 0)] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

validTransferOwnerScenario :: (Monad m) => NettestImpl m -> m ()
validTransferOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner1
          , #txs .! [ (#to_ .! owner2, (#token_id .! 0, #amount .! 10)) ]
          )
        ]
  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
  -- Check the balance
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved owner2) dao (Call @"Balance_of")
    (mkFA2View [ (#owner .! owner2, #token_id .! 0)] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((owner2, 0 :: Natural), 110 :: Natural)]] )

updatingOperatorScenario :: (Monad m) => NettestImpl m -> m ()
updatingOperatorScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDAO
  let params = (#owner .! owner1, (#operator .! owner2, #token_id .! 0))
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.Add_operator params]
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.Remove_operator params]

  let notOwnerParams = (#owner .! owner2, (#operator .! owner1, #token_id .! 0))

  callFrom (AddressResolved owner1) dao (Call @"Update_operators") ([FA2.Add_operator notOwnerParams])
    & expectCustomError_ #nOT_OWNER

  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.Remove_operator notOwnerParams]
    & expectCustomError_ #nOT_OWNER

lowBalanceScenario :: (Monad m) => NettestImpl m -> m ()
lowBalanceScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner1
          , #txs .! [ (#to_ .! owner2, (#token_id .! 0, #amount .! 200)) ]
          )
        ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)

lowBalanceOwnerScenario :: (Monad m) => NettestImpl m -> m ()
lowBalanceOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner1
          , #txs .! [ (#to_ .! owner2, (#token_id .! 0, #amount .! 200)) ]
          )
        ]
  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 200, #present .! 100)

noSourceAccountScenario :: (Monad m) => NettestImpl m -> m ()
noSourceAccountScenario = uncapsNettest $ do
  nonexistent :: Address <- newAddress "nonexistent"
  ((owner1, _), _, dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! nonexistent
          , #txs .! [ (#to_ .! owner1, (#token_id .! 0, #amount .! 1)) ]
          )
        ]
  -- Failed with 'fA2_INSUFFICIENT_BALANCE' due to nonexistent account is treated
  -- as an existing account with 0 balance.
  callFrom (AddressResolved nonexistent) dao (Call @"Transfer") params
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0)

badOperatorScenario :: (Monad m) => NettestImpl m -> m ()
badOperatorScenario = uncapsNettest $ do
  op3  :: Address <- newAddress "operator3"
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner2
          , #txs .! [ (#to_ .! owner1, (#token_id .! 0, #amount .! 0)) ]
          )
        ]
  callFrom (AddressResolved op3) dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

emptyTransferListScenario :: (Monad m) => NettestImpl m -> m ()
emptyTransferListScenario = uncapsNettest $ do
  ((_, op1), _, dao, _) <- originateBaseDAO
  callFrom (AddressResolved op1) dao (Call @"Transfer") []

validateTokenScenario :: (Monad m) => NettestImpl m -> m ()
validateTokenScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDAO
  let params tokenId =
        [ ( #from_ .! owner1
          , #txs .! [ (#to_ .! owner2, (#token_id .! tokenId, #amount .! 10)) ]
          )
        ]
      callWith = callFrom (AddressResolved op1) dao (Call @"Transfer")

  callWith (params 0)
  -- TODO: Add `fROZEN_TOKEN_NOT_TRANSFERABLE` to spec.
  callWith (params 1)
    & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
  callWith (params 2)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

validateTokenOwnerScenario :: (Monad m) => NettestImpl m -> m ()
validateTokenOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDAO
  let params tokenId =
        [ ( #from_ .! owner1
          , #txs .! [ (#to_ .! owner2, (#token_id .! tokenId, #amount .! 10)) ]
          )
        ]
      callWith = callFrom (AddressResolved owner1) dao (Call @"Transfer")

  callWith (params 0)
  callWith (params 1)
    & expectCustomError_ #fROZEN_TOKEN_NOT_TRANSFERABLE
  callWith (params 2)
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

noForeignMoneyScenario :: (Monad m) => NettestImpl m -> m ()
noForeignMoneyScenario = uncapsNettest $ do
  ((owner1, op1), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner2
          , #txs .! [ (#to_ .! owner1, (#token_id .! 0, #amount .! 10)) ]
          )
        ]
  callFrom (AddressResolved op1) dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

noForeignMoneyOwnerScenario :: (Monad m) => NettestImpl m -> m ()
noForeignMoneyOwnerScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <- originateBaseDAO
  let params =
        [ ( #from_ .! owner2
          , #txs .! [ (#to_ .! owner1, (#token_id .! 0, #amount .! 10)) ]
          )
        ]
  callFrom (AddressResolved owner1) dao (Call @"Transfer") params
    & expectCustomError_ #fA2_NOT_OPERATOR

balanceOfOwnerScenario :: (Monad m) => NettestImpl m -> m ()
balanceOfOwnerScenario = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDAO
  consumer <- originateSimple "consumer" [] contractConsumer
  let
    params requestItems = mkFA2View requestItems consumer
    callWith = callFrom (AddressResolved owner1) dao (Call @"Balance_of")

  callWith (params [ (#owner .! owner1, #token_id .! 0)])
  callWith (params [ (#owner .! owner1, #token_id .! 1)])
  callWith (params [])
  callWith (params [ (#owner .! owner1, #token_id .! 2)])
    & expectCustomError_ #fA2_TOKEN_UNDEFINED

adminTransferScenario :: (Monad m) => NettestImpl m -> m ()
adminTransferScenario = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDAO
  let params = [
          ( #from_ .! owner2
          , #txs .! [ (#to_ .! owner1, (#token_id .! 0, #amount .! 10)) ]
          )
        ]
  callFrom (AddressResolved admin) dao (Call @"Transfer") params

-------------------------------------------------
-- Helper
-------------------------------------------------

mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)
