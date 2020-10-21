-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO
  ( Config(..)
  , defaultConfig

  , Ledger
  , Operators
  , Parameter (..)
  , Storage (..)

  , baseDaoContract
  , emptyStorage
  , mkStorage
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.FA2
import Lorentz.Contracts.BaseDAO.Management
import Lorentz.Contracts.BaseDAO.Proposal
import Lorentz.Contracts.BaseDAO.Types as BaseDAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

baseDaoContract
  :: forall pm.
      ( NiceParameter pm, TypeHasDoc pm
      , HasAnnotation pm, TypeHasDoc (Proposal pm)
      , NicePackedValue pm
      )
  => Config pm -> Contract (BaseDAO.Parameter pm) (BaseDAO.Storage pm)
baseDaoContract config@Config{..} = defaultContract $ contractName cDaoName $ do
  doc $ DDescription cDaoDescription
  ensureZeroTransfer
  unpair
  entryCase @(BaseDAO.Parameter pm) (Proxy @PlainEntrypointsKind)
    ( #cCall_FA2 /-> fa2Handler
    , #cTransfer_ownership /-> transferOwnership
    , #cAccept_ownership /-> acceptOwnership
    , #cMigrate /-> migrate
    , #cConfirm_migration /-> confirmMigration
    , #cPropose /-> propose config
    , #cProposal_metadata /-> proposalMetadata
    , #cVote /-> vote config
    , #cSet_voting_period /-> setVotingPeriod config
    , #cSet_quorum_threshold /-> setQuorumThreshold config
    , #cFlush /-> flush config
    )

fa2Handler :: (IsoValue pm) => Entrypoint FA2.Parameter (BaseDAO.Storage pm)
fa2Handler =
  entryCase @FA2.Parameter (Proxy @PlainEntrypointsKind)
    ( #cTransfer /-> transfer
    , #cBalance_of /-> balanceOf
    , #cToken_metadata_registry /-> tokenMetadataRegistry
    , #cUpdate_operators /-> updateOperators
    )

ensureZeroTransfer :: s :-> s
ensureZeroTransfer = do
  amount
  push zeroMutez
  if IsEq then nop else failCustom_ #fORBIDDEN_XTZ
