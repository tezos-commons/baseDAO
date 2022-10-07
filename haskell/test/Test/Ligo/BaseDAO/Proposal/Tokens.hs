-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

-- | Contains tests on @propose@ entrypoint logic for behavior around
-- tokens, freezing/unfreezing etc.
module Test.Ligo.BaseDAO.Proposal.Tokens
  ( canUnfreezeFromPreviousPeriod
  , cannotUnfreezeFromSamePeriod
  , cannotUnfreezeStakedTokens
  , checkFreezeHistoryTracking
  , freezeTokens
  ) where

import Universum

import Data.ByteString qualified as BS
import Morley.Tezos.Address
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

freezeTokens
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
freezeTokens originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ transfer dodDao $ calling (ep @"Freeze") (#amount :! 10)
  checkBalance dodDao dodOwner1 10
  -- Check that the FA2 token got a transfer call as expected.

  tcStorage <- getStorage @[[FA2.TransferItem]] dodTokenContract
  assert (tcStorage ==
    ([[FA2.TransferItem
      { tiFrom = MkAddress dodOwner1
      , tiTxs = [FA2.TransferDestination { tdTo = MkAddress $ chAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])) "Unexpected FA2 transfers"

checkFreezeHistoryTracking
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
checkFreezeHistoryTracking originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let frozen_scale_value = 2
  let frozen_extra_value = 10
  let proposalMeta1 = ""
  let proposalSize1 = fromIntegral . BS.length $ proposalMeta1
  let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Freeze") (#amount :! requiredFrozen)
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Propose") (ProposeParams (MkAddress dodOwner1) requiredFrozen proposalMeta1)
  advanceToLevel dodPeriod

  fh <- getFreezeHistory dodDao dodOwner1
  let expected = AddressFreezeHistory
        { fhCurrentStageNum = 1
        , fhCurrentUnstaked = 0
        , fhStaked = requiredFrozen
        , fhPastUnstaked = 0
        }

  assert (fh == (Just expected)) "Unexpected freeze history"

canUnfreezeFromPreviousPeriod
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
canUnfreezeFromPreviousPeriod originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Freeze") (#amount :! 10)
  checkBalance dodDao dodOwner1 10

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Unfreeze") (#amount :! 10)
  checkBalance dodDao dodOwner1 00
  -- Check that the FA2 token got a transfer call as expected.
  tcStorage <- getStorage @[[FA2.TransferItem]] dodTokenContract
  assert (tcStorage ==
    ([ [ FA2.TransferItem
        { tiFrom = MkAddress $ chAddress dodDao
        , tiTxs = [FA2.TransferDestination { tdTo = MkAddress dodOwner1, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
        }]
      , [FA2.TransferItem
        { tiFrom = MkAddress $ dodOwner1
        , tiTxs = [FA2.TransferDestination { tdTo = MkAddress $ chAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])) "Unexpected FA2 transfers"

cannotUnfreezeStakedTokens
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
cannotUnfreezeStakedTokens originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Freeze") (#amount :! 50)
  checkBalance dodDao dodOwner1 50

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  void $ createSampleProposal 1 dodOwner1 dodDao

  -- the frozen tokens are still the same
  checkBalance dodDao dodOwner1 50
  -- but unfreeze won't let all of them be unfrozen because of the staked tokens
  -- note: 110 tokens are staked here
  withSender dodOwner1 $ (transfer dodDao$ calling (ep @"Unfreeze") (#amount :! 41))
    & expectFailedWith notEnoughFrozenTokens
  -- it will allow the un-staked ones to be unfrozen
  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Unfreeze") (#amount :! 40)

cannotUnfreezeFromSamePeriod
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
cannotUnfreezeFromSamePeriod originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ transfer dodDao$ calling (ep @"Freeze") (#amount :! 10)
  checkBalance dodDao dodOwner1 10

  -- Cannot unfreeze in the same period
  withSender dodOwner1 $ (transfer dodDao$ calling (ep @"Unfreeze") (#amount :! 10))
    & expectFailedWith notEnoughFrozenTokens
