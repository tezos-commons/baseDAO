-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( getFullStorage
  , getFullStorageView

  , GetTotalSupplyFn
  , getTotalSupplyEmulator

  , GetProposalFn
  , getProposalEmulator

  , GetFreezeHistoryFn
  , getFreezeHistoryEmulator

  , GetQuorumThresholdAtCycleFn
  , getQtAtCycleEmulator
  -- , getTotalSupplyNetwork

  , GetVotePermitsCounterFn
  , getVotePermitsCounterEmulator
  -- , getVotePermitsCounterNetwork
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import qualified Data.Map as M
import Morley.Nettest
import Morley.Nettest.Pure (PureM)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Ligo.BaseDAO.Types

getFullStorage :: Address -> EmulatedT PureM FullStorage
getFullStorage addr =
  fromVal @FullStorage <$> getStorage' @(ToT FullStorage) addr

getFullStorageView :: (Monad m) => Address -> NettestT m FullStorageView
getFullStorageView addr =
  fromVal @FullStorageView <$> getStorage @(ToT FullStorageView) addr

------------------------------------------------------------------------
-- GetTotalSupply
------------------------------------------------------------------------

type GetTotalSupplyFn m = Address -> FA2.TokenId -> m Natural

getTotalSupplyEmulator :: Address -> FA2.TokenId -> EmulatedT PureM Natural
getTotalSupplyEmulator addr tokenId = do
  fs <- getFullStorage addr
  let result = case M.lookup tokenId $ sTotalSupply (fsStorage fs) of
        Just v -> v
        Nothing -> error "getTotalSupply: token_id does not exist."
  pure result

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

-- getTotalSupplyNetwork :: (Monad m) => Address -> FA2.TokenId -> NettestT m Natural
-- getTotalSupplyNetwork addr tokenId = do
--   fs <- getFullStorageView addr
--   let result = case M.lookup tokenId $ sTotalSupply (fsStorage fs) of
--         Just v -> v
--         Nothing -> error "getTotalSupply: token_id does not exist."
--   pure result

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
