-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( checkBalance
  , checkGuardian
  , checkIfAProposalExist
  , checkIfDelegateExists
  , getFreezeHistory
  , getFrozenTotalSupply
  , getFullStorage
  , getProposal
  , getProposalStartLevel
  , getVoter
  , getQtAtCycle
  , getStorageRPC
  , getStorageRPC'
  , getVotePermitsCounter
  , getOriginationLevel
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import qualified Data.Set as S
import Morley.Michelson.Typed.Scope (HasNoBigMap, HasNoTicket, HasNoContract, HasNoNestedBigMaps, HasNoOp)
import Test.Cleveland

import Ligo.BaseDAO.Types

type CEConatraints cep =
  ( Typeable cep
  , Typeable (VariantToExtra cep)
  , HasNoTicket (ToT (AsRPC (VariantToExtra cep)))
  , HasNoBigMap (ToT (AsRPC (VariantToExtra cep)))
  , HasNoOp (ToT (AsRPC (VariantToExtra cep)))
  , HasNoContract (ToT (AsRPC (VariantToExtra cep)))
  , HasNoNestedBigMaps (ToT (AsRPC (VariantToExtra cep)))
  , HasNoTicket (ToT (VariantToExtra cep))
  , HasNoBigMap (ToT (VariantToExtra cep))
  , HasNoOp (ToT (VariantToExtra cep))
  , HasNoContract (ToT (VariantToExtra cep))
  , HasNoNestedBigMaps (ToT (VariantToExtra cep))
  , HasAnnotation (VariantToExtra cep)
  , IsoValue (VariantToExtra cep)
  , IsoValue (AsRPC (VariantToExtra cep))
  , HasNoOp (ToT (VariantToExtra cep))
  )

getStorageRPC' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p ->  m (FullStorageRPC' (VariantToExtra cep))
getStorageRPC' addr = getStorage @(FullStorageSkeleton (VariantToExtra cep)) (unTAddress addr)

getStorageRPC :: forall p base caps m. MonadCleveland caps base m => TAddress p ->  m FullStorageRPC
getStorageRPC addr = getStorageRPC' @'Base addr

getFrozenTotalSupply' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p -> m Natural
getFrozenTotalSupply' addr = (sFrozenTotalSupplyRPC . fsStorageRPC) <$> (getStorageRPC' @cep addr)

getFrozenTotalSupply :: forall p base caps m. MonadCleveland caps base m => TAddress p -> m Natural
getFrozenTotalSupply addr = getFrozenTotalSupply' @'Base addr

getFreezeHistory' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p -> Address -> m (Maybe AddressFreezeHistory)
getFreezeHistory' addr owner = do
  freezeHistoryBmId <- (sFreezeHistoryRPC . fsStorageRPC) <$> (getStorageRPC' @cep addr)
  getBigMapValueMaybe freezeHistoryBmId owner

getFreezeHistory :: forall p base caps m. (MonadCleveland caps base m) => TAddress p -> Address -> m (Maybe AddressFreezeHistory)
getFreezeHistory addr owner = getFreezeHistory' @'Base addr owner

getQtAtCycle' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p -> m QuorumThresholdAtCycle
getQtAtCycle' addr = (sQuorumThresholdAtCycleRPC . fsStorageRPC) <$> getStorageRPC' @cep addr

getQtAtCycle :: forall p base caps m. MonadCleveland caps base m => TAddress p -> m QuorumThresholdAtCycle
getQtAtCycle addr = (sQuorumThresholdAtCycleRPC . fsStorageRPC) <$> getStorageRPC addr

getProposal'
  :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m)
  => TAddress p
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal' addr pKey = do
  bId <- (sProposalsRPC . fsStorageRPC) <$> getStorageRPC' @cep addr
  getBigMapValueMaybe bId pKey

getProposal
  :: forall p base caps m. MonadCleveland caps base m
  => TAddress p
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal addr pKey = getProposal' @Base addr pKey

getVoter'
  :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m)
  => TAddress p
  -> (Address, ProposalKey)
  -> m (Maybe StakedVote)
getVoter' addr key = do
  bId <- (sStakedVotesRPC . fsStorageRPC) <$> getStorageRPC addr
  getBigMapValueMaybe bId key

getVoter
  :: forall p base caps m. MonadCleveland caps base m
  => TAddress p
  -> (Address, ProposalKey)
  -> m (Maybe StakedVote)
getVoter addr key = getVoter' @'Base addr key

getProposalStartLevel'
  :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m)
  => TAddress p
  -> ProposalKey
  -> m Natural
getProposalStartLevel' addr pKey =
   plStartLevel . fromMaybe (error "proposal not found") <$> getProposal' @cep addr pKey

getProposalStartLevel
  :: forall p base caps m. (MonadCleveland caps base m)
  => TAddress p
  -> ProposalKey
  -> m Natural
getProposalStartLevel addr pKey = getProposalStartLevel' @'Base addr pKey

checkIfDelegateExists'
  :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m)
  => TAddress p
  -> Delegate
  -> m Bool
checkIfDelegateExists' addr delegate = do
  bId <- (sDelegatesRPC . fsStorageRPC) <$> getStorageRPC' @cep addr
  isJust <$> getBigMapValueMaybe bId delegate

checkIfDelegateExists
  :: forall p base caps m. MonadCleveland caps base m
  => TAddress p
  -> Delegate
  -> m Bool
checkIfDelegateExists addr delegate = checkIfDelegateExists' @'Base addr delegate

checkIfAProposalExist'
  :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m)
  => ProposalKey -> TAddress p -> Bool -> m ()
checkIfAProposalExist' proposalKey dodDao expected = do
  found <- getProposal' @cep dodDao proposalKey >>= \case
    Nothing -> pure False
    Just p -> do
      proposalSet <- (sProposalKeyListSortByDateRPC . fsStorageRPC) <$> getStorageRPC dodDao
      pure $ S.member (plStartLevel p, proposalKey) proposalSet
  assert (found == expected) $
    "Unexpected proposal status, expected:" <> (show expected) <> ", found: " <> (show found)

checkIfAProposalExist
  :: forall p base caps m. MonadCleveland caps base m
  => ProposalKey -> TAddress p -> Bool -> m ()
checkIfAProposalExist = checkIfAProposalExist' @'Base

checkGuardian' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p -> Address -> m ()
checkGuardian' addr guardianToChk = do
  actual <- (sGuardianRPC . fsStorageRPC) <$> (getStorageRPC' @cep addr)
  actual @== guardianToChk

checkGuardian :: forall p base caps m. MonadCleveland caps base m => TAddress p -> Address -> m ()
checkGuardian = checkGuardian' @'Base

checkBalance'
  :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m)
  => TAddress p
  -> Address
  -> Natural
  -> m ()
checkBalance' addr owner bal = do
  fh <- getFreezeHistory' @cep addr owner
  (sumAddressFreezeHistory <$> fh) @== Just bal

checkBalance
  :: forall p base caps m. MonadCleveland caps base m
  => TAddress p
  -> Address
  -> Natural
  -> m ()
checkBalance = checkBalance' @'Base

sumAddressFreezeHistory :: AddressFreezeHistory -> Natural
sumAddressFreezeHistory AddressFreezeHistory{..} = fhCurrentUnstaked + fhPastUnstaked + fhStaked

getVotePermitsCounter' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p ->  m Nonce
getVotePermitsCounter' addr =
  (sPermitsCounterRPC . fsStorageRPC) <$> getStorageRPC' @cep addr

getVotePermitsCounter :: forall p base caps m. MonadCleveland caps base m => TAddress p ->  m Nonce
getVotePermitsCounter = getVotePermitsCounter' @'Base

getOriginationLevel' :: forall cep p base caps m. (CEConatraints cep, MonadCleveland caps base m) => TAddress p ->  m Natural
getOriginationLevel' dodDao = (sStartLevelRPC . fsStorageRPC) <$> (getStorageRPC' @cep dodDao)

getOriginationLevel :: forall p base caps m. MonadCleveland caps base m => TAddress p ->  m Natural
getOriginationLevel = getOriginationLevel' @'Base
