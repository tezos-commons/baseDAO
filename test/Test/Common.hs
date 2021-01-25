-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE NumericUnderscores #-}

module Test.Common
  ( originateTrivialDao
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
import Morley.Nettest
import Named (defaults, (!))
import Util.Named ((.!))

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
      { odFrom = nettestAddress
      , odName = "BaseDAO"
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

makeProposalKey :: NicePackedValue pm => DAO.ProposeParams pm -> Address -> ProposalKey pm
makeProposalKey params owner = toHashHs $ lPackValue (params, owner)
