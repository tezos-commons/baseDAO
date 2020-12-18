-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE NumericUnderscores #-}

module Test.Common
  ( mkFA2View
  , checkTokenBalance
  , withOriginated
  , originateTrivialDao
  , originateBaseDaoWithConfig
  , originateBaseDaoWithBalance
  , originateTrivialDaoWithBalance
  , makeProposalKey
  , addDataToSign
  , permitProtect
  , sendXtz
  ) where

import Universum

import qualified Data.Map as M
import qualified Data.Set as S
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Named (defaults, (!))
import Util.Named ((.!))

import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.TrivialDAO as DAO

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> (Storage () ()))
  -> ([Address] -> TAddress (Parameter () Empty) -> m a)
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
  :: forall pm op ce caps base m.
     (DAO.DaoC ce pm op, MonadNettest caps base m)
  => ce -> DAO.Config ce pm op -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter pm op), Address)
originateBaseDaoWithConfig contractExtra config = do
  originateBaseDaoWithBalance contractExtra config
    (\owner1 owner2 ->
        [ ((owner1, unfrozenTokenId), 100)
        , ((owner2, unfrozenTokenId), 100)
        ]
    )

originateBaseDaoWithBalance
  :: forall pm op ce caps base m.
     (DAO.DaoC ce pm op, MonadNettest caps base m)
  => ce
  -> DAO.Config ce pm op
  -> (Address -> Address -> [((Address, FA2.TokenId), Natural)])
  -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter pm op), Address)
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
      , odStorage =
          ( mkStorage
          ! #admin admin
          ! #extra contractExtra
          ! #votingPeriod 20
          ! #quorumThreshold 1
          ! #metadata mempty
          ! defaults
          ) { sLedger = BigMap bal
            , sOperators = BigMap operators
            }
      , odContract = DAO.baseDaoContract config
      }
  dao <- originate originateData

  pure ((owner1, operator1), (owner2, operator2), dao, admin)

originateTrivialDaoWithBalance
  :: (MonadNettest caps base m)
  => (Address -> Address -> [((Address, FA2.TokenId), Natural)])
  -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter () Empty), Address)
originateTrivialDaoWithBalance =
  originateBaseDaoWithBalance () DAO.trivialConfig

originateBaseDao
  :: forall pm op ce caps base m.
     (DAO.DaoC ce pm op, MonadNettest caps base m)
  => ce -> m ((Address, Address), (Address, Address), TAddress (DAO.Parameter pm op), Address)
originateBaseDao contractExtra = originateBaseDaoWithConfig contractExtra DAO.defaultConfig

originateTrivialDao
  :: (MonadNettest caps base m)
  => m ((Address, Address), (Address, Address), TAddress (DAO.Parameter () Empty), Address)
originateTrivialDao = originateBaseDao ()

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: (NiceParameterFull (Parameter pm op), MonadNettest caps base m)
  => FA2.TokenId -> TAddress (DAO.Parameter pm op)
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

makeProposalKey :: NicePackedValue pm => DAO.ProposeParams pm -> Address -> ProposalKey pm
makeProposalKey params owner = toHashHs $ lPackValue (params, owner)

addDataToSign
  :: (MonadNettest caps base m)
  => TAddress (Parameter pm op)
  -> Nonce
  -> d
  -> m (DataToSign d, d)
addDataToSign (toAddress -> dsContract) dsNonce dsData = do
  dsChainId <- getChainId
  return (DataToSign{..}, dsData)

-- | Add a permit from given user.
permitProtect
  :: (MonadNettest caps base m, NicePackedValue a)
  => AddressOrAlias -> (DataToSign a, a) -> m (PermitProtected a)
permitProtect author (toSign, a) = do
  authorAlias <- getAlias author
  pKey <- getPublicKey author
  pSignature <- signBinary (lPackValue toSign) authorAlias
  return PermitProtected
    { ppArgument = a
    , ppPermit = Just Permit{..}
    }

sendXtz :: MonadNettest caps base m => Address -> m ()
sendXtz addr = do
  let transferData = TransferData
        { tdFrom = nettestAddress
        , tdTo = AddressResolved addr
        , tdAmount = toMutez 0.5_e6 -- 0.5 xtz
        , tdEntrypoint = DefEpName
        , tdParameter = ()
        }
  transfer transferData
