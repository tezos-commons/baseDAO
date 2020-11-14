-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Common
  ( mkFA2View
  , checkTokenBalance
  , withOriginated
  , originateBaseDao
  , originateBaseDaoWithConfig
  , originateBaseDaoWithBalance
  , TestProposalMetadata
  ) where

import Universum

-- import qualified Data.Map.Strict as Map
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Named (defaults, (!))
-- import Util.Named ((.!))

import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> (Storage ()))
  -> ([Address] -> TAddress (Parameter ()) -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  baseDao <- originate $ OriginateData
    { odFrom = nettestAddress
    , odName = "BaseDAO Test Contract"
    , odBalance = zeroMutez
    , odStorage = storageFn addresses
    , odContract = DAO.baseDaoContract DAO.defaultConfig
    }
  tests addresses baseDao

type TestProposalMetadata = Integer

-- | Helper functions which originate BaseDAO with a predefined owners, operators and initial storage.
-- used in FA2 Proposals tests.
originateBaseDaoWithConfig
  :: MonadNettest caps base m
  => DAO.Config TestProposalMetadata -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter TestProposalMetadata), Address)
originateBaseDaoWithConfig config = do
  originateBaseDaoWithBalance config
    (\owner1 owner2 ->
        [ ((owner1, 0), 100)
        , ((owner2, 0), 100)
        ]
    )

originateBaseDaoWithBalance
  :: MonadNettest caps base m
  => DAO.Config TestProposalMetadata
  -> (Address -> Address -> [((Address, FA2.TokenId), Natural)])
  -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter TestProposalMetadata), Address)
originateBaseDaoWithBalance config balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let
    originateData = OriginateData
      { odFrom = nettestAddress
      , odName = "BaseDAO"
      , odBalance = toMutez 0
      -- , odStorage = (mkStorage ! #admin ! defaults) {
      --  sLedger = BigMap bal
      --  sOperators = operators
      -- }
      , odStorage = mkStorage ! #admin admin ! defaults
      , odContract = DAO.baseDaoContract config
      }
  dao <- originate originateData

  -- Add operators
  callFrom (AddressResolved owner1) dao (Call @"Update_operators") [FA2.AddOperator
    (FA2.OperatorParam
        { opOwner = owner1
        , opOperator = operator1
        , opTokenId = 0
        }
    )]
  callFrom (AddressResolved owner2) dao (Call @"Update_operators") [FA2.AddOperator
    (FA2.OperatorParam
        { opOwner = owner2
        , opOperator = operator2
        , opTokenId = 0
        }
    )]

  -- Initial balances
  let bal = balFunc owner1 owner2
  mapM_
    (\((owner, tokenId), amount_) ->
        callFrom (AddressResolved admin) dao (Call @"Mint") (MintParam owner tokenId amount_)
    ) bal

  pure ((owner1, operator1), (owner2, operator2), dao, admin)

originateBaseDao
  :: MonadNettest caps base m
  => m ((Address, Address), (Address, Address), TAddress (DAO.Parameter TestProposalMetadata), Address)
originateBaseDao = originateBaseDaoWithConfig DAO.defaultConfig

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: MonadNettest caps base m
  => FA2.TokenId -> TAddress (DAO.Parameter TestProposalMetadata)
  -> Address -> Natural
  -> m ()
checkTokenBalance tokenId dao addr expectedValue = do
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved addr) dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = addr
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((addr, tokenId), expectedValue)]] )
