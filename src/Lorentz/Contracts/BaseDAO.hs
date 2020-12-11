-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO
  ( Config(..)
  , defaultConfig

  , Ledger
  , Operators
  , Parameter (..)
  , Storage (..)

  , DaoContract
  , baseDaoContract
  , mkStorage
  ) where

import Lorentz

import Lorentz.Contracts.BaseDAO.Doc
import Lorentz.Contracts.BaseDAO.Management
import Lorentz.Contracts.BaseDAO.Proposal
import Lorentz.Contracts.BaseDAO.Token
import Lorentz.Contracts.BaseDAO.Token.FA2
import Lorentz.Contracts.BaseDAO.Types as BaseDAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

data FA2EntrypointsKind
instance EntrypointKindHasDoc FA2EntrypointsKind where
  entrypointKindPos = 1055
  entrypointKindSectionName = "FA2 entrypoints"

type DaoContract ce pm =
  Contract (BaseDAO.Parameter pm) (BaseDAO.Storage ce pm)

baseDaoContract
  :: forall ce pm.
      ( NiceParameter pm, TypeHasDoc pm
      , HasAnnotation pm
      , NicePackedValue pm
      , KnownValue ce, TypeHasDoc ce
      )
  => Config ce pm -> DaoContract ce pm
baseDaoContract config@Config{..} = defaultContract $ contractName cDaoName $ do
  contractGeneralDefault
  doc $ DDescription $ cDaoDescription <> "\n\n" <> introductoryDoc
  docStorage @(BaseDAO.Storage ce pm)

  entrypointSection "Prior checks" (Proxy @CommonContractBehaviourKind) $ do
    doc $ DDescription priorChecksDoc
    ensureZeroTransfer

  pushFuncContext config $ do
    unpair
    finalizeParamCallingDoc $ entryCase @(BaseDAO.Parameter pm) (Proxy @PlainEntrypointsKind)
      ( #cCall_FA2 /-> fa2Handler
      , #cTransfer_ownership /-> transferOwnership
      , #cAccept_ownership /-> acceptOwnership
      , #cMigrate /-> migrate
      , #cConfirm_migration /-> confirmMigration
      , #cPropose /-> propose config
      , #cVote /-> vote config
      , #cSet_voting_period /-> setVotingPeriod config
      , #cSet_quorum_threshold /-> setQuorumThreshold config
      , #cFlush /-> flush config
      , #cBurn /-> burn
      , #cMint /-> mint
      , #cTransfer_contract_tokens /-> transferContractTokens
      , #cToken_address /-> tokenAddress

      , #cGetVotePermitCounter /-> view_ $ do
          doc $ DDescription getVotePermitCounterDoc
          drop @(); stToField #sPermitsCounter
      )

fa2Handler
  :: ( IsoValue ce, KnownValue pm
     , HasFuncContext s (BaseDAO.Storage ce pm) (TransferFuncs (BaseDAO.Storage ce pm))
     )
  => Entrypoint' FA2.Parameter (BaseDAO.Storage ce pm) s
fa2Handler = do
  doc $ DDescription callFA2Doc
  entryCase @FA2.Parameter (Proxy @FA2EntrypointsKind)
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

pushFuncContext
  :: (KnownValue ce, KnownValue pm, NiceParameter pm, store ~ BaseDAO.Storage ce pm)
  => Config ce pm
  -> (forall s. HasFuncContext s store (AllFuncs store ce pm) =>
      (inp : s) :-> (out : s)
     )
  -> Lambda inp out
pushFuncContext config action = do
  dip $ do
    pushCachedFuncSimple creditTo
    pushCachedFuncSimple debitFrom
    pushCachedFunc (unfreezeProposerToken config)
      do dupLNamed #creditTo; applicate
         dupLNamed #debitFrom; applicate
      do dug @6; dug @6
      do dipN @4 $ dropN @2

  action

  dip $ dropN @3
