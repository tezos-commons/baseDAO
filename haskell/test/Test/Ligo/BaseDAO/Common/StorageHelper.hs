-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( getFullStorage
  , getFullStorageView

  , GetFrozenTotalSupplyFn
  , getFrozenTotalSupplyEmulator

  , GetQuorumThresholdAtCycleFn
  , getQtAtCycleEmulator

  , CheckGuardianFn
  , GetProposalFn
  , checkGuardianEmulator
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

import Ligo.BaseDAO.Types

getFullStorage :: MonadEmulated caps base m => Address -> m FullStorage
getFullStorage addr =
  fromVal @FullStorage <$> getStorage' @(ToT FullStorage) addr

getFullStorageView :: (Monad m) => Address -> NettestT m FullStorageView
getFullStorageView addr =
  fromVal @FullStorageView <$> getStorage @(ToT FullStorageView) addr


------------------------------------------------------------------------
-- GetFrozenTotalSupplyFn
------------------------------------------------------------------------

type GetFrozenTotalSupplyFn m = Address -> m Natural

getFrozenTotalSupplyEmulator :: MonadEmulated caps base m => Address -> m Natural
getFrozenTotalSupplyEmulator addr = do
  fs <- getFullStorage addr
  pure $ sFrozenTotalSupply $ fsStorage fs

------------------------------------------------------------------------
-- GetFreezeHistoryFn
------------------------------------------------------------------------

type GetFreezeHistoryFn m = Address -> Address -> m (Maybe AddressFreezeHistory)

getFreezeHistoryEmulator :: MonadEmulated caps base m => Address -> Address -> m (Maybe AddressFreezeHistory)
getFreezeHistoryEmulator addr owner =
  (M.lookup owner . unBigMap . sFreezeHistory . fsStorage) <$> getFullStorage addr

type CheckGuardianFn m = Address -> Address -> m ()

checkGuardianEmulator :: MonadEmulated caps base m => Address -> Address -> m ()
checkGuardianEmulator addr guardianToChk = do
  actual <- (sGuardian . fsStorage) <$> (getFullStorage addr)
  actual @== guardianToChk

type GetQuorumThresholdAtCycleFn m = Address -> m QuorumThresholdAtCycle

getQtAtCycleEmulator :: MonadEmulated caps base m => Address -> m QuorumThresholdAtCycle
getQtAtCycleEmulator addr = (sQuorumThresholdAtCycle . fsStorage) <$> getFullStorage addr

type GetProposalFn m = Address -> ProposalKey -> m (Maybe Proposal)

getProposalEmulator :: MonadEmulated caps base m => Address -> ProposalKey -> m (Maybe Proposal)
getProposalEmulator addr pKey =
  (M.lookup pKey . unBigMap . sProposals . fsStorage) <$> getFullStorage addr


-- | Note: Not needed at the moment, due to all the tests that uses this run only in emulator
-- anyway. Commented due to weeder.

------------------------------------------------------------------------
-- CheckBalanceFn
------------------------------------------------------------------------

type CheckBalanceFn m = Address -> Address -> Natural -> m ()

checkBalanceEmulator :: MonadEmulated caps base m => Address -> Address -> Natural -> m ()
checkBalanceEmulator addr owner bal = do
  fh <- getFreezeHistoryEmulator addr owner
  (sumAddressFreezeHistory <$> fh) @== Just bal

sumAddressFreezeHistory :: AddressFreezeHistory -> Natural
sumAddressFreezeHistory AddressFreezeHistory{..} = fhCurrentUnstaked + fhPastUnstaked + fhStaked

------------------------------------------------------------------------
-- GetVotePermitsCounter
------------------------------------------------------------------------

type GetVotePermitsCounterFn m = Address -> m Nonce

getVotePermitsCounterEmulator :: MonadEmulated caps base m => Address -> m Nonce
getVotePermitsCounterEmulator addr = do
  fs <- getFullStorage addr
  pure $ sPermitsCounter (fsStorage fs)

-- | Note: Not needed at the moment, due to all the tests that uses this run only in emulator
-- anyway. Commented due to weeder.

-- getVotePermitsCounterNetwork :: (Monad m) => Address -> NettestT m Nonce
-- getVotePermitsCounterNetwork addr = do
--   fs <- getFullStorageView addr
--   pure $ sPermitsCounter (fsStorage fs)
