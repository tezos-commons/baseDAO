-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE NumericUnderscores #-}

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Common
  ( checkTokenBalance
  , originateTrivialDao
  , originateBaseDaoWithConfig
  , originateBaseDaoWithBalance
  , originateTrivialDaoWithBalance
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
import Util.Named ((.!))

import BaseDAO.ShareTest.Common (totalSupplyFromLedger)
import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.TrivialDAO as DAO

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

-- TODO: [#96] Add a way to customize storage
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
      { odName = "BaseDAO"
      , odBalance = toMutez 0
      , odStorage =
          ( mkStorage
          ! #admin admin
          ! #extra contractExtra
          ! #votingPeriod (cMinVotingPeriod config)
          ! #quorumThreshold (cMinQuorumThreshold config)
          ! #metadata mempty
          ! defaults
          ) { sLedger = BigMap bal
            , sTotalSupply = totalSupplyFromLedger (BigMap bal)
            , sOperators = BigMap operators
            }
      , odContract = DAO.baseDaoContract config
      }
  dao <- originateLarge originateData

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

  withSender (AddressResolved addr) $ call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = addr
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((addr, tokenId), expectedValue)]] )

makeProposalKey :: NicePackedValue pm => DAO.ProposeParams pm -> Address -> ProposalKey pm
makeProposalKey params owner = toHashHs $ lPackValue (params, owner)
