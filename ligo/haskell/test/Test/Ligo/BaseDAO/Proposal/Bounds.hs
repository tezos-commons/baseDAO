-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on proposal/vote limits logic for testing the Ligo contract.
module Test.Ligo.BaseDAO.Proposal.Bounds
  ( setVotingPeriod
  , setQuorumThreshold
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest

import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config
import Ligo.BaseDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

setVotingPeriod
  :: forall pm param config caps base m.
    ( MonadNettest caps base m
    , ParameterC param pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
setVotingPeriod _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  let param = 60 * 60 -- 1 hour

  withSender (AddressResolved owner1) $
    call dao (Call @"Set_voting_period") param
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $
    call dao (Call @"Set_voting_period") param
  -- TODO [#31]: checkStorage

setQuorumThreshold
  :: forall pm param config caps base m.
    ( MonadNettest caps base m
    , ParameterC param pm
    , AllConfigDescsDefined config
    , HasCallStack
    )
  => IsLorentz -> (ConfigDesc config -> OriginateFn param m) -> m ()
setQuorumThreshold _ originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn testConfig

  let param = 100

  withSender (AddressResolved owner1) $
    call dao (Call @"Set_quorum_threshold") param
    & expectCustomErrorNoArg #nOT_ADMIN

  withSender (AddressResolved admin) $
    call dao (Call @"Set_quorum_threshold") param
  -- TODO [#31]: checkStorage

