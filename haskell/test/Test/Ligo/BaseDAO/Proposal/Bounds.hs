-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on proposal/vote limits logic for testing the Ligo contract.
module Test.Ligo.BaseDAO.Proposal.Bounds
  ( setVotingPeriod
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest

import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config
import Ligo.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

setVotingPeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
setVotingPeriod originateFn = do
  ((owner1, _), _, dao, _, admin) <- originateFn testConfig

  let param = 60 * 60 -- 1 hour

  withSender (AddressResolved owner1) $
    call dao (Call @"Set_voting_period") param
    & expectCustomErrorNoArg #nOT_ADMIN dao

  withSender (AddressResolved admin) $
    call dao (Call @"Set_voting_period") param
  -- TODO [#31]: checkStorage
