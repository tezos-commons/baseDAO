-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE NumericUnderscores #-}

module Test.Ligo.BaseDAO.Common
  ( createSampleProposal
  , originateLigoDaoWithBalance
  , originateLigoDaoWithConfigDesc
  , originateLigoDao
  ) where

import Universum

import Named (defaults, (!))

import Lorentz hiding (now)
import Michelson.Typed.Convert (convertContract, untypeValue)
import Morley.Nettest
import Time (sec)
import Util.Named

import Ligo.BaseDAO.ShareTest.Common
  (OriginateFn, totalSupplyFromLedger, makeProposalKey, proposalMetadataFromNum)
import Ligo.BaseDAO.ShareTest.Proposal.Config ()
import qualified Data.Map as M
import qualified Data.Set as S
import Ligo.BaseDAO.ConfigDesc
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types

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

  now <- getNow

  let fullStorage = FullStorage
        { fsStorage =
            ( mkStorageL
              ! #extra extra
              ! #admin admin
              ! #votingPeriod (cMinVotingPeriod configL)
              ! #quorumThreshold (cMinQuorumThreshold configL)
              ! #metadata mempty
              ! #now now
              ! defaults
            )
            { sLedger = bal
            , sOperators = operators
            , sTotalSupply = totalSupplyFromLedger bal
            }
        , fsConfig = configL
        }

  let
    originateData = UntypedOriginateData
      { uodName = "BaseDAO"
      , uodBalance = toMutez 0
      , uodStorage = untypeValue $ toVal $ fullStorage
      , uodContract = convertContract baseDAOContractLigo
      }
  daoUntyped <- originateLargeUntyped originateData
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

createSampleProposal
  :: ( MonadNettest caps base m
     , HasCallStack
     )
  => Int -> Int -> Address -> TAddress ParameterL -> m (ProposalKey ProposalMetadataL)
createSampleProposal counter vp owner1 dao = do
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum counter
        }

  when (vp > 0) $ do
    withSender (AddressResolved owner1) $
      call dao (Call @"Freeze") (#amount .! 10)
    advanceTime (sec (fromIntegral vp))

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)
