-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Common
  ( originateLigoDao
  , originateLigoDaoWithBalance
  ) where

import Universum

import Lorentz
import Morley.Nettest
import Util.Named

import Lorentz.Contracts.BaseDAO.Types

import qualified Ligo.BaseDAO.Contract as Ligo
import qualified Ligo.BaseDAO.Types as Ligo
import qualified Ligo.BaseDAO.Helper as Ligo
import Michelson.Typed.Convert (convertContract, untypeValue)
import qualified Data.Map as M
import qualified Data.Set as S

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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
