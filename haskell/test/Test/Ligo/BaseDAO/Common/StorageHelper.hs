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

import Test.Cleveland

import Ligo.BaseDAO.Types

getStorageRPC'
  :: forall cep caps m p s.
     ( HasCallStack, CEConstraints cep, MonadCleveland caps m
     , NiceStorage (VariantToExtra cep)
     )
  => ContractHandle p s () -> m (StorageSkeletonRPC (VariantToExtra cep))
getStorageRPC' addr = getStorage @(StorageSkeleton (VariantToExtra cep)) (chAddress addr)

getStorageRPC :: forall p s caps m. (HasCallStack, MonadCleveland caps m) => ContractHandle p s () ->  m StorageRPC
getStorageRPC addr = getStorageRPC' @'Base addr

getFrozenTotalSupply' :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m) => ContractHandle p s () -> m Natural
getFrozenTotalSupply' addr = sFrozenTotalSupplyRPC <$> (getStorageRPC' @cep addr)

getFrozenTotalSupply :: forall p s caps m. MonadCleveland caps m => ContractHandle p s () -> m Natural
getFrozenTotalSupply addr = getFrozenTotalSupply' @'Base addr

getFreezeHistory'
  :: forall cep p s caps m addr. (CEConstraints cep, MonadCleveland caps m, ToImplicitAddress addr)
  => ContractHandle p s () -> addr -> m (Maybe AddressFreezeHistory)
getFreezeHistory' addr (toImplicitAddress -> owner) = do
  freezeHistoryBmId <- sFreezeHistoryRPC <$> (getStorageRPC' @cep addr)
  getBigMapValueMaybe freezeHistoryBmId (toAddress owner)

getFreezeHistory
  :: forall p s caps m addr. (MonadCleveland caps m, ToImplicitAddress addr)
  => ContractHandle p s () -> addr -> m (Maybe AddressFreezeHistory)
getFreezeHistory addr owner = getFreezeHistory' @'Base addr owner

getQtAtCycle'
  :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m)
  => ContractHandle p s () -> m QuorumThresholdAtCycle
getQtAtCycle' addr = sQuorumThresholdAtCycleRPC <$> getStorageRPC' @cep addr

getQtAtCycle :: forall p s caps m. MonadCleveland caps m => ContractHandle p s () -> m QuorumThresholdAtCycle
getQtAtCycle = getQtAtCycle' @'Base

getProposal'
  :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m)
  => ContractHandle p s ()
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal' addr pKey = do
  bId <- sProposalsRPC <$> getStorageRPC' @cep addr
  getBigMapValueMaybe bId pKey

getProposal
  :: forall p s caps m. MonadCleveland caps m
  => ContractHandle p s ()
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal addr pKey = getProposal' @'Base addr pKey

getVoter
  :: forall p s caps m addr. (MonadCleveland caps m, ToAddress addr)
  => ContractHandle p s ()
  -> (addr, ProposalKey)
  -> m (Maybe StakedVote)
getVoter addr key = do
  bId <- sStakedVotesRPC <$> getStorageRPC addr
  getBigMapValueMaybe bId $ first toAddress key

getProposalStartLevel'
  :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m)
  => ContractHandle p s ()
  -> ProposalKey
  -> m Natural
getProposalStartLevel' addr pKey =
   plStartLevel . fromMaybe (error "proposal not found") <$> getProposal' @cep addr pKey

getProposalStartLevel
  :: forall p s caps m. (MonadCleveland caps m)
  => ContractHandle p s ()
  -> ProposalKey
  -> m Natural
getProposalStartLevel addr pKey = getProposalStartLevel' @'Base addr pKey

checkIfDelegateExists'
  :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m)
  => ContractHandle p s ()
  -> Delegate
  -> m Bool
checkIfDelegateExists' addr delegate = do
  bId <- sDelegatesRPC <$> getStorageRPC' @cep addr
  isJust <$> getBigMapValueMaybe bId delegate

checkIfDelegateExists
  :: forall p s caps m. MonadCleveland caps m
  => ContractHandle p s ()
  -> Delegate
  -> m Bool
checkIfDelegateExists addr delegate = checkIfDelegateExists' @'Base addr delegate

plistMemRPC'
  :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m)
  => ContractHandle p s () -> ProposalKey -> m Bool
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
  :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m)
  => ProposalKey -> ContractHandle p s () -> Bool -> m ()
checkIfAProposalExist' proposalKey dodDao expected = do
  found <- getProposal' @cep dodDao proposalKey >>= \case
    Nothing -> pure False
    Just _ -> do
      plistMemRPC' @cep dodDao proposalKey
  assert (found == expected) $
    "Unexpected proposal status, expected:" <> (show expected) <> ", found: " <> (show found)

checkIfAProposalExist
  :: forall p s caps m. MonadCleveland caps m
  => ProposalKey -> ContractHandle p s () -> Bool -> m ()
checkIfAProposalExist = checkIfAProposalExist' @'Base

checkGuardian'
  :: forall cep p s caps m addr. (CEConstraints cep, MonadCleveland caps m, ToAddress addr)
  => ContractHandle p s () -> addr -> m ()
checkGuardian' addr guardianToChk = do
  actual <- sGuardianRPC <$> (getStorageRPC' @cep addr)
  actual @== toAddress guardianToChk

checkBalance'
  :: forall cep p s caps m addr. (CEConstraints cep, MonadCleveland caps m, ToImplicitAddress addr)
  => ContractHandle p s ()
  -> addr
  -> Natural
  -> m ()
checkBalance' addr owner bal = do
  fh <- getFreezeHistory' @cep addr owner
  (sumAddressFreezeHistory <$> fh) @== Just bal

checkBalance
  :: forall p s caps m addr. (MonadCleveland caps m, ToImplicitAddress addr)
  => ContractHandle p s ()
  -> addr
  -> Natural
  -> m ()
checkBalance = checkBalance' @'Base

sumAddressFreezeHistory :: AddressFreezeHistory -> Natural
sumAddressFreezeHistory AddressFreezeHistory{..} = fhCurrentUnstaked + fhPastUnstaked + fhStaked

getVotePermitsCounter' :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m) => ContractHandle p s () ->  m Nonce
getVotePermitsCounter' addr =
  sPermitsCounterRPC <$> getStorageRPC' @cep addr

getVotePermitsCounter :: forall p s caps m. MonadCleveland caps m => ContractHandle p s () ->  m Nonce
getVotePermitsCounter = getVotePermitsCounter' @'Base

getOriginationLevel' :: forall cep p s caps m. (CEConstraints cep, MonadCleveland caps m) => ContractHandle p s () ->  m Natural
getOriginationLevel' dodDao = sStartLevelRPC <$> (getStorageRPC' @cep dodDao)

getOriginationLevel :: forall p s caps m. MonadCleveland caps m => ContractHandle p s () ->  m Natural
getOriginationLevel = getOriginationLevel' @'Base
