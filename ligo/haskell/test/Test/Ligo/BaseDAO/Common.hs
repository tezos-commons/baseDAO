-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Common
  ( originateLigoDaoWithBalance
  , originateLigoDaoWithConfig
  , originateLigoDao
  ) where

import Universum

import Named (defaults, (!))

import Lorentz
import Morley.Nettest
import Util.Named

import BaseDAO.ShareTest.Common
import qualified Data.Map as M
import qualified Data.Set as S
import Ligo.BaseDAO.ConfigDesc
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Michelson.Typed.Convert (convertContract, untypeValue)

originateLigoDaoWithBalance
 :: forall caps base m. (MonadNettest caps base m)
 => ContractExtraL
 -> ConfigDesc ConfigL
 -> (Address -> Address -> [(LedgerKey, LedgerValue)])
 -> OriginateFn ParameterL m
originateLigoDaoWithBalance extra configDesc balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let config = fillConfig configDesc defaultConfigL

  let bal = M.fromList $ balFunc owner1 owner2
  let operators = M.fromSet (const ()) $ S.fromList
        [ (owner1, operator1)
        , (owner2, operator2)
        ]

  let fullStorage = FullStorage
        { fsStorage =
            ( mkStorageL
              ! #extra extra
              ! #admin admin
              ! #votingPeriod (cMinVotingPeriod config)
              ! #quorumThreshold (cMinQuorumThreshold config)
              ! #metadata mempty
              ! defaults
            )
            { sLedger = bal
            , sOperators = operators
            }
        , fsConfig = config
        }

  let
    originateData = UntypedOriginateData
      { uodFrom = nettestAddress
      , uodName = "BaseDAO"
      , uodBalance = toMutez 0
      , uodStorage = untypeValue $ toVal $ fullStorage
      , uodContract = convertContract baseDAOContractLigo
      }
  daoUntyped <- originateUntyped originateData
  let dao = TAddress @ParameterL daoUntyped

  pure ((owner1, operator1), (owner2, operator2), dao, admin)

originateLigoDaoWithConfig
 :: forall caps base m. (MonadNettest caps base m)
 => ConfigDesc ConfigL
 -> OriginateFn ParameterL m
originateLigoDaoWithConfig config =
  originateLigoDaoWithBalance dynRecUnsafe config
    (\owner1_ owner2_ ->
    [ ((owner1_, unfrozenTokenId), 100)
    , ((owner2_, unfrozenTokenId), 100)
    ])

originateLigoDao
 :: forall caps base m. (MonadNettest caps base m)
 => OriginateFn ParameterL m
originateLigoDao =
  originateLigoDaoWithConfig (ConfigDesc ())
