-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Common
  ( originateLigoDaoWithBalance
  , originateLigoDaoWithConfigDesc
  , originateLigoDao
  ) where

import Universum

import Named (defaults, (!))

import Lorentz
import Morley.Nettest
import Util.Named

import BaseDAO.ShareTest.Common (OriginateFn)
import BaseDAO.ShareTest.Proposal.Config ()
import qualified Data.Map as M
import qualified Data.Set as S
import Ligo.BaseDAO.ConfigDesc
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Michelson.Typed.Convert (convertContract, untypeValue)

originateLigoDaoWithBalance
 :: forall caps base m. (MonadNettest caps base m)
 => ContractExtraL
 -> ConfigL
 -> (Address -> Address -> [(LedgerKey, LedgerValue)])
 -> OriginateFn ParameterL m
originateLigoDaoWithBalance extra configL balFunc = do
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

  let fullStorage = FullStorage
        { fsStorage =
            ( mkStorageL
              ! #extra extra
              ! #admin admin
              ! #votingPeriod (cMinVotingPeriod configL)
              ! #quorumThreshold (cMinQuorumThreshold configL)
              ! #metadata mempty
              ! defaults
            )
            { sLedger = bal
            , sOperators = operators
            }
        , fsConfig = configL
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
 => ContractExtraL
 -> ConfigL
 -> OriginateFn ParameterL m
originateLigoDaoWithConfig extra configL =
  originateLigoDaoWithBalance extra configL
    (\owner1_ owner2_ ->
    [ ((owner1_, unfrozenTokenId), 100)
    , ((owner2_, unfrozenTokenId), 100)
    ])

originateLigoDaoWithConfigDesc
 :: forall caps base m. (MonadNettest caps base m)
 => ContractExtraL
 -> ConfigDesc ConfigL
 -> OriginateFn ParameterL m
originateLigoDaoWithConfigDesc extra config =
  originateLigoDaoWithBalance extra (fillConfig config defaultConfigL)
    (\owner1_ owner2_ ->
    [ ((owner1_, unfrozenTokenId), 100)
    , ((owner2_, unfrozenTokenId), 100)
    ])

originateLigoDao
 :: forall caps base m. (MonadNettest caps base m)
 => OriginateFn ParameterL m
originateLigoDao =
  originateLigoDaoWithConfig dynRecUnsafe defaultConfigL
