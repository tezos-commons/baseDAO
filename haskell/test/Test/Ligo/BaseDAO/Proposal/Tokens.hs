-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoint logic for behavior around
-- tokens, freezing/unfreezing etc.
module Test.Ligo.BaseDAO.Proposal.Tokens
  ( canUnfreezeFromPreviousPeriod
  , cannotUnfreezeFromSamePeriod
  , cannotUnfreezeStakedTokens
  , freezeTokens
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

freezeTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
freezeTokens originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 110
  -- Check that the FA2 token got a transfer call as expected.
  checkStorage (unTAddress dodTokenContract)
    (toVal [[FA2.TransferItem
      { tiFrom = dodOwner1
      , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])

canUnfreezeFromPreviousPeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
canUnfreezeFromPreviousPeriod originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Advance one voting period to a proposing stage.
  advanceLevel 15

  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 100
  -- Check that the FA2 token got a transfer call as expected.
  checkStorage (unTAddress dodTokenContract)
    (toVal
      [ [ FA2.TransferItem
        { tiFrom = unTAddress dodDao
        , tiTxs = [FA2.TransferDestination { tdTo = dodOwner1, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
        }]
      , [FA2.TransferItem
        { tiFrom = dodOwner1
        , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])

cannotUnfreezeStakedTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
cannotUnfreezeStakedTokens originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 50)
  checkTokenBalance frozenTokenId dodDao dodOwner1 150

  -- Advance one voting period to a proposing stage.
  advanceLevel 15
  void $ createSampleProposal 1 dodOwner1 dodDao

  -- the frozen tokens are still the same
  checkTokenBalance frozenTokenId dodDao dodOwner1 150
  -- but unfreeze won't let all of them be unfrozen because of the staked tokens
  -- note: 110 tokens are staked here
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 41)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao
  -- it will allow the un-staked ones to be unfrozen
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 40)

cannotUnfreezeFromSamePeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
cannotUnfreezeFromSamePeriod originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Cannot unfreeze in the same period
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao

