-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( getFullStorage
  , getFullStorageView

  , GetFrozenTotalSupplyFn
  , getFrozenTotalSupplyEmulator

  , GetQuorumThresholdAtCycleFn
  , getQtAtCycleEmulator

  , GetProposalFn
  , getProposalEmulator

  , GetFreezeHistoryFn
  , getFreezeHistoryEmulator

  , CheckBalanceFn
  , checkBalanceEmulator

  , GetVotePermitsCounterFn
  , getVotePermitsCounterEmulator
  -- , getVotePermitsCounterNetwork
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import qualified Data.Map as M
import Morley.Nettest
import Morley.Nettest.Pure (PureM)

import Ligo.BaseDAO.Types

getFullStorage :: Address -> EmulatedT PureM FullStorage
getFullStorage = getStorage' @FullStorage

getFullStorageView :: (Monad m, HasAnnotation FullStorageView) => Address -> NettestT m FullStorageView
getFullStorageView = getStorage @FullStorageView


------------------------------------------------------------------------
-- GetFrozenTotalSupplyFn
------------------------------------------------------------------------

type GetFrozenTotalSupplyFn m = Address -> m Natural

getFrozenTotalSupplyEmulator :: Address -> EmulatedT PureM Natural
getFrozenTotalSupplyEmulator addr = do
  fs <- getFullStorage addr
  pure $ sFrozenTotalSupply $ fsStorage fs

------------------------------------------------------------------------
-- GetFreezeHistoryFn
------------------------------------------------------------------------

type GetFreezeHistoryFn m = Address -> Address -> m (Maybe AddressFreezeHistory)

getFreezeHistoryEmulator :: Address -> Address -> EmulatedT PureM (Maybe AddressFreezeHistory)
getFreezeHistoryEmulator addr owner =
  (M.lookup owner . unBigMap . sFreezeHistory . fsStorage) <$> getFullStorage addr

type GetQuorumThresholdAtCycleFn m = Address -> m QuorumThresholdAtCycle

getQtAtCycleEmulator :: Address -> EmulatedT PureM QuorumThresholdAtCycle
getQtAtCycleEmulator addr = (sQuorumThresholdAtCycle . fsStorage) <$> getFullStorage addr

type GetProposalFn m = Address -> ProposalKey -> m (Maybe Proposal)

getProposalEmulator :: Address -> ProposalKey -> EmulatedT PureM (Maybe Proposal)
getProposalEmulator addr pKey =
  (M.lookup pKey . unBigMap . sProposals . fsStorage) <$> getFullStorage addr


-- | Note: Not needed at the moment, due to all the tests that uses this run only in emulator
-- anyway. Commented due to weeder.

------------------------------------------------------------------------
-- CheckBalanceFn
------------------------------------------------------------------------

type CheckBalanceFn m = Address -> Address -> Natural -> m ()

checkBalanceEmulator :: Address -> Address -> Natural -> EmulatedT PureM ()
checkBalanceEmulator addr owner bal = do
  fh <- getFreezeHistoryEmulator addr owner
  (sumAddressFreezeHistory <$> fh) @== Just bal

sumAddressFreezeHistory :: AddressFreezeHistory -> Natural
sumAddressFreezeHistory AddressFreezeHistory{..} = fhCurrentUnstaked + fhPastUnstaked + fhStaked

------------------------------------------------------------------------
-- GetVotePermitsCounter
------------------------------------------------------------------------

type GetVotePermitsCounterFn m = Address -> m Nonce

getVotePermitsCounterEmulator :: Address -> EmulatedT PureM Nonce
getVotePermitsCounterEmulator addr = do
  fs <- getFullStorage addr
  pure $ sPermitsCounter (fsStorage fs)

-- | Note: Not needed at the moment, due to all the tests that uses this run only in emulator
-- anyway. Commented due to weeder.

-- getVotePermitsCounterNetwork :: (Monad m) => Address -> NettestT m Nonce
-- getVotePermitsCounterNetwork addr = do
--   fs <- getFullStorageView addr
--   pure $ sPermitsCounter (fsStorage fs)
