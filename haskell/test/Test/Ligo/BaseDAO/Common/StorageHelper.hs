-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( checkBalance
  , checkBalance'
  , checkGuardian'
  , checkIfAProposalExist
  , checkIfAProposalExist'
  , checkIfDelegateExists
  , getFreezeHistory
  , getFrozenTotalSupply
  , getProposal
  , getProposalStartLevel
  , getProposalStartLevel'
  , getVoter
  , getQtAtCycle
  , getStorageRPC
  , getVotePermitsCounter
  , getOriginationLevel
  , getOriginationLevel'
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import Morley.Michelson.Typed.Scope
  (HasNoBigMap, HasNoContract, HasNoNestedBigMaps, HasNoOp, HasNoTicket)
import Test.Cleveland

import Ligo.BaseDAO.Types

type CEConatraints cep =
  ( Typeable cep
  , Typeable (VariantToExtra cep)
  , Typeable (AsRPC (VariantToExtra cep))
  , HasNoTicket (ToT (AsRPC (VariantToExtra cep)))
  , HasNoBigMap (ToT (AsRPC (VariantToExtra cep)))
  , HasNoOp (ToT (AsRPC (VariantToExtra cep)))
  , HasNoContract (ToT (AsRPC (VariantToExtra cep)))
  , HasNoNestedBigMaps (ToT (AsRPC (VariantToExtra cep)))
  , HasNoTicket (ToT (VariantToExtra cep))
  , HasNoOp (ToT (VariantToExtra cep))
  , HasNoContract (ToT (VariantToExtra cep))
  , HasNoNestedBigMaps (ToT (VariantToExtra cep))
  , HasAnnotation (VariantToExtra cep)
  , IsoValue (VariantToExtra cep)
  , IsoValue (AsRPC (VariantToExtra cep))
  , HasNoOp (ToT (VariantToExtra cep))
  )

getStorageRPC' :: forall cep p vd caps m. (HasCallStack, CEConatraints cep, MonadCleveland caps m) => TAddress p vd ->  m (StorageSkeletonRPC (AsRPC (VariantToExtra cep)))
getStorageRPC' addr = getStorage @(StorageSkeleton (VariantToExtra cep)) (unTAddress addr)

getStorageRPC :: forall p caps vd m. (HasCallStack, MonadCleveland caps m) => TAddress p vd ->  m StorageRPC
getStorageRPC addr = getStorageRPC' @'Base addr

getFrozenTotalSupply' :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m) => TAddress p vd -> m Natural
getFrozenTotalSupply' addr = sFrozenTotalSupplyRPC <$> (getStorageRPC' @cep addr)

getFrozenTotalSupply :: forall p vd caps m. MonadCleveland caps m => TAddress p vd -> m Natural
getFrozenTotalSupply addr = getFrozenTotalSupply' @'Base addr

getFreezeHistory' :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m) => TAddress p vd -> Address -> m (Maybe AddressFreezeHistory)
getFreezeHistory' addr owner = do
  freezeHistoryBmId <- sFreezeHistoryRPC <$> (getStorageRPC' @cep addr)
  getBigMapValueMaybe freezeHistoryBmId owner

getFreezeHistory :: forall p vd caps m. (MonadCleveland caps m) => TAddress p vd -> Address -> m (Maybe AddressFreezeHistory)
getFreezeHistory addr owner = getFreezeHistory' @'Base addr owner

getQtAtCycle' :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m) => TAddress p vd -> m QuorumThresholdAtCycle
getQtAtCycle' addr = sQuorumThresholdAtCycleRPC <$> getStorageRPC' @cep addr

getQtAtCycle :: forall p vd caps m. MonadCleveland caps m => TAddress p vd -> m QuorumThresholdAtCycle
getQtAtCycle = getQtAtCycle' @'Base

getProposal'
  :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m)
  => TAddress p vd
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal' addr pKey = do
  bId <- sProposalsRPC <$> getStorageRPC' @cep addr
  getBigMapValueMaybe bId pKey

getProposal
  :: forall p vd caps m. MonadCleveland caps m
  => TAddress p vd
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal addr pKey = getProposal' @'Base addr pKey

getVoter
  :: forall p vd caps m. MonadCleveland caps m
  => TAddress p vd
  -> (Address, ProposalKey)
  -> m (Maybe StakedVote)
getVoter addr key = do
  bId <- sStakedVotesRPC <$> getStorageRPC addr
  getBigMapValueMaybe bId key

getProposalStartLevel'
  :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m)
  => TAddress p vd
  -> ProposalKey
  -> m Natural
getProposalStartLevel' addr pKey =
   plStartLevel . fromMaybe (error "proposal not found") <$> getProposal' @cep addr pKey

getProposalStartLevel
  :: forall p vd caps m. (MonadCleveland caps m)
  => TAddress p vd
  -> ProposalKey
  -> m Natural
getProposalStartLevel addr pKey = getProposalStartLevel' @'Base addr pKey

checkIfDelegateExists'
  :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m)
  => TAddress p vd
  -> Delegate
  -> m Bool
checkIfDelegateExists' addr delegate = do
  bId <- sDelegatesRPC <$> getStorageRPC' @cep addr
  isJust <$> getBigMapValueMaybe bId delegate

checkIfDelegateExists
  :: forall p vd caps m. MonadCleveland caps m
  => TAddress p vd
  -> Delegate
  -> m Bool
checkIfDelegateExists addr delegate = checkIfDelegateExists' @'Base addr delegate

plistMemRPC'
  :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m)
  => TAddress p vd -> ProposalKey -> m Bool
plistMemRPC' addr key = do
  plistMb <- sOngoingProposalsDlistRPC <$> getStorageRPC' @cep addr
  case plistMb of
    Just ProposalDoublyLinkedListRPC{..} ->
      if (plFirstRPC == key)
        then pure True
        else do
          result <- getBigMapValueMaybe plMapRPC (key, False)
          case result of
            Nothing -> pure False
            Just _ -> pure True
    Nothing -> pure False

checkIfAProposalExist'
  :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m)
  => ProposalKey -> TAddress p vd -> Bool -> m ()
checkIfAProposalExist' proposalKey dodDao expected = do
  found <- getProposal' @cep dodDao proposalKey >>= \case
    Nothing -> pure False
    Just _ -> do
      plistMemRPC' @cep dodDao proposalKey
  assert (found == expected) $
    "Unexpected proposal status, expected:" <> (show expected) <> ", found: " <> (show found)

checkIfAProposalExist
  :: forall p vd caps m. MonadCleveland caps m
  => ProposalKey -> TAddress p vd -> Bool -> m ()
checkIfAProposalExist = checkIfAProposalExist' @'Base

checkGuardian' :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m) => TAddress p vd -> Address -> m ()
checkGuardian' addr guardianToChk = do
  actual <- sGuardianRPC <$> (getStorageRPC' @cep addr)
  actual @== guardianToChk

checkBalance'
  :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m)
  => TAddress p vd
  -> Address
  -> Natural
  -> m ()
checkBalance' addr owner bal = do
  fh <- getFreezeHistory' @cep addr owner
  (sumAddressFreezeHistory <$> fh) @== Just bal

checkBalance
  :: forall p vd caps m. MonadCleveland caps m
  => TAddress p vd
  -> Address
  -> Natural
  -> m ()
checkBalance = checkBalance' @'Base

sumAddressFreezeHistory :: AddressFreezeHistory -> Natural
sumAddressFreezeHistory AddressFreezeHistory{..} = fhCurrentUnstaked + fhPastUnstaked + fhStaked

getVotePermitsCounter' :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m) => TAddress p vd ->  m Nonce
getVotePermitsCounter' addr =
  sPermitsCounterRPC <$> getStorageRPC' @cep addr

getVotePermitsCounter :: forall p vd caps m. MonadCleveland caps m => TAddress p vd ->  m Nonce
getVotePermitsCounter = getVotePermitsCounter' @'Base

getOriginationLevel' :: forall cep p vd caps m. (CEConatraints cep, MonadCleveland caps m) => TAddress p vd ->  m Natural
getOriginationLevel' dodDao = sStartLevelRPC <$> (getStorageRPC' @cep dodDao)

getOriginationLevel :: forall p vd caps m. MonadCleveland caps m => TAddress p vd ->  m Natural
getOriginationLevel = getOriginationLevel' @'Base
