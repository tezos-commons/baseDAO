-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Common
  ( mkFA2View
  , checkTokenBalance
  , withOriginated
  , originateBaseDao
  , originateBaseDaoWithConfig
  , originateBaseDaoWithBalance
  , makeProposalKey
  ) where

import Universum

import qualified Data.Map as M
import qualified Data.Set as S
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Named (defaults, (!))
import Tezos.Crypto (blake2b)
import Util.Named ((.!))

import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.BaseDAO.Types as DAO

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> (Storage () ()))
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

-- | Helper functions which originate BaseDAO with a predefined owners, operators and initial storage.
-- used in FA2 Proposals tests.
originateBaseDaoWithConfig
  :: forall pm ce caps base m.
     ( NiceStorage (Storage ce pm), NiceParameterFull (Parameter pm), NiceStorage pm
     , TypeHasDoc (Proposal pm), TypeHasDoc pm, NicePackedValue pm
     , KnownValue ce, TypeHasDoc ce
     , MonadNettest caps base m
     )
  => ce -> DAO.Config ce pm -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter pm), Address)
originateBaseDaoWithConfig contractExtra config = do
  originateBaseDaoWithBalance contractExtra config
    (\owner1 owner2 ->
        [ ((owner1, 0), 100)
        , ((owner2, 0), 100)
        ]
    )

originateBaseDaoWithBalance
  :: forall pm ce caps base m.
     ( NiceStorage (Storage ce pm), NiceParameterFull (Parameter pm), NiceStorage pm
     , TypeHasDoc (Proposal pm), TypeHasDoc pm, NicePackedValue pm
     , KnownValue ce, TypeHasDoc ce
     , MonadNettest caps base m
     )
  => ce
  -> DAO.Config ce pm
  -> (Address -> Address -> [((Address, FA2.TokenId), Natural)])
  -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter pm), Address)
originateBaseDaoWithBalance contractExtra config balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let bal = M.fromList $ balFunc owner1 owner2
  let operators = M.fromSet (const ()) $ S.fromList
        [ (#owner .! owner1, #operator .! operator1)
        , (#owner .! owner2, #operator .! operator2)
        ]

  let
    originateData = OriginateData
      { odFrom = nettestAddress
      , odName = "BaseDAO"
      , odBalance = toMutez 0
      , odStorage = (mkStorage ! #admin admin ! #extra contractExtra ! defaults)
        { sLedger = BigMap bal
        , sOperators = BigMap operators
        }
      , odContract = DAO.baseDaoContract config
      }
  dao <- originate originateData

  pure ((owner1, operator1), (owner2, operator2), dao, admin)

originateBaseDao
  :: forall pm ce caps base m.
     ( NiceStorage (Storage ce pm), NiceParameterFull (Parameter pm), NiceStorage pm
     , TypeHasDoc (Proposal pm), TypeHasDoc pm, NicePackedValue pm
     , KnownValue ce, TypeHasDoc ce
     , MonadNettest caps base m
     )
  => ce -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter pm), Address)
originateBaseDao contractExtra = originateBaseDaoWithConfig contractExtra DAO.defaultConfig

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: (NiceParameterFull (Parameter pm), MonadNettest caps base m)
  => FA2.TokenId -> TAddress (DAO.Parameter pm)
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

makeProposalKey :: NicePackedValue pm => DAO.ProposeParams pm -> Address -> ByteString
makeProposalKey params owner = blake2b $ lPackValue (params, owner)
