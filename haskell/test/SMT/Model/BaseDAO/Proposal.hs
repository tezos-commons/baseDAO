-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.Model.BaseDAO.Proposal
  ( applyFreeze
  , applyUnfreeze
  , applyVote
  , applyPropose
  , applyFlush
  , applyDropProposal
  , applyUpdateDelegate
  , applyUnstakeVote
  ) where

import Universum

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Natural

import Lorentz hiding (cast, div, get, not, or, take)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed.Haskell.Value (BigMap(..))
import Morley.Util.Named

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Permit
import SMT.Model.BaseDAO.Proposal.FreezeHistory
import SMT.Model.BaseDAO.Proposal.Plist
import SMT.Model.BaseDAO.Proposal.QuorumThreshold
import SMT.Model.BaseDAO.Token
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

checkIfProposalExist
  :: ProposalKey -> ModelT cep Proposal
checkIfProposalExist key = do
  store <- getStore

  let resultMaybe = do
        p <- Map.lookup key (store & sProposals & bmMap)
        if plistMem key (store & sOngoingProposalsDlist) then
          Just p
        else Nothing

  case resultMaybe of
    Just p -> pure p
    Nothing -> throwError PROPOSAL_NOT_EXIST


checkDelegate
  :: Address -> Address -> ModelT cep Address
checkDelegate from author  = do
  store <- getStore
  let key = Delegate {dOwner = from, dDelegate = author}
  if (author /= from) && not (Map.member key (store & sDelegates & bmMap)) then
    throwError NOT_DELEGATE
  else pure from

stakeTk :: Natural -> Address -> ModelT var ()
stakeTk tokenAmount addr = do
  stakingUpdateFh addr $ fromIntegral tokenAmount
  modifyStore $ \s -> do
    let newCycleStaked = (s & sQuorumThresholdAtCycle & qaStaked) + tokenAmount
    pure $ s
      { sQuorumThresholdAtCycle = (s & sQuorumThresholdAtCycle) { qaStaked = newCycleStaked}
      }

ensureProposingStage :: Natural -> ModelT cep ()
ensureProposingStage stageNum = do
  unless (stageNum `mod` 2 == 1) $
    throwError NOT_PROPOSING_STAGE


ensureProposalIsUnique :: ProposeParams -> ModelT cep ProposalKey
ensureProposalIsUnique params = do
  store <- getStore
  let proposalKey = makeProposalKey params
  case Map.member proposalKey (store & sProposals & bmMap) of
    True -> throwError PROPOSAL_NOT_UNIQUE
    False -> pure proposalKey


addProposal :: ProposeParams -> ModelT cep ()
addProposal params = do
  lvl <- get <&> msLevel
  proposalKey <- ensureProposalIsUnique params
  currentStage <- getCurrentStageNum
  ensureProposingStage currentStage

  store <- getStore
  let proposal = Proposal
        { plUpvotes = 0
        , plDownvotes = 0
        , plStartLevel = lvl
        , plVotingStageNum = currentStage + 1
        , plMetadata = params & ppProposalMetadata
        , plProposer = params & ppFrom
        , plProposerFrozenToken = params & ppFrozenToken
        , plQuorumThreshold = store & sQuorumThresholdAtCycle & qaQuorumThreshold
        }
  modifyStore $ \s -> pure $ s
    { sProposals = BigMap Nothing $ Map.insert proposalKey proposal (bmMap (s & sProposals))
    , sOngoingProposalsDlist = plistInsert proposalKey (s & sOngoingProposalsDlist)
    }

unstakeTk :: Natural -> Natural -> Address -> ModelT cep ()
unstakeTk tokenAmount burnAmount addr = do
  store <- getStore

  stakingUpdateFh addr (negate $ fromIntegral tokenAmount)
  burnUpdateFh addr burnAmount

  newTotalSupply <- case (store & sFrozenTotalSupply) `minusNaturalMaybe` burnAmount of
    Just n -> pure n
    Nothing -> error "BAD_STATE: Trying to burn more than `sFrozenTotalSupply`."

  modifyStore $ \s -> pure $ s
    { sFrozenTotalSupply = newTotalSupply
    }


doTotalVoteMeetQuorumThreshold :: Proposal -> StorageSkeleton var -> Bool
doTotalVoteMeetQuorumThreshold proposal store =
  let votesPlaced = (proposal & plUpvotes) + (proposal & plDownvotes)
      totalSupply = store & sFrozenTotalSupply
      reachedQuorum = QuorumThreshold $ (votesPlaced * fromIntegral fractionDenominator) `div` totalSupply
  in  (reachedQuorum >= (proposal & plQuorumThreshold))

unstakeProposerToken
  :: Bool -> Proposal -> ModelT cep ()
unstakeProposerToken isAccepted proposal = do
  store <- getStore
  (FixedFee fixedFee) <- getConfig <&> cFixedProposalFee
  (tokens, burnAmount) <-
      if isAccepted then pure ((proposal & plProposerFrozenToken) + fixedFee, 0)
      else do
        slashAmount <- get <&> msRejectedProposalSlashValue >>= \f -> f (proposal, store & sExtra)
        let frozenTokens = (proposal & plProposerFrozenToken) + fixedFee
            desiredBurnAmount = slashAmount + fixedFee
            tokens = case frozenTokens `minusNaturalMaybe` desiredBurnAmount of
              Just value -> value
              Nothing -> 0
        pure (tokens, desiredBurnAmount)

  unstakeTk tokens burnAmount (proposal & plProposer)


applyPropose :: ModelSource -> ProposeParams -> ModelT var ()
applyPropose mso param@ProposeParams{..} = do

  config <- getConfig
  let FixedFee fixedFee = config & cFixedProposalFee

  validFrom <- checkDelegate ppFrom (toAddress $ mso & msoSender)

  store <- getStore
  get <&> msProposalCheck >>= \f -> f (param, (store & sExtra))

  let amountToFreeze = ppFrozenToken + fixedFee
  currentStage <- getCurrentStageNum

  updateQuorum currentStage
  stakeTk amountToFreeze validFrom
  addProposal param

ensureProposalVotingStage :: Proposal -> ModelT cep ()
ensureProposalVotingStage proposal = do
  currentStage <- getCurrentStageNum
  unless (currentStage == (proposal & plVotingStageNum)) $
    throwError VOTING_STAGE_OVER

submitVote :: Proposal -> VoteParam -> Address -> ModelT cep ()
submitVote proposal voteParam author = do
  store <- getStore
  let proposalKey = voteParam & vProposalKey
      stakeAmt = case Map.lookup (author, proposalKey) (store & sStakedVotes & bmMap) of
        Just vs -> vs
        Nothing -> 0

      newStakeAmt = stakeAmt + (voteParam & vVoteAmount)

      updatedProposal = if (voteParam & vVoteType)
                        then proposal { plUpvotes = (proposal & plUpvotes) + (voteParam & vVoteAmount)}
                        else proposal { plDownvotes = (proposal & plDownvotes) + (voteParam & vVoteAmount)}
  stakeTk (voteParam & vVoteAmount) author
  modifyStore $ \s -> pure $ s
    { sProposals = BigMap Nothing $ Map.insert proposalKey updatedProposal (s & sProposals & bmMap)
    , sStakedVotes = BigMap Nothing $ Map.insert (author, proposalKey) newStakeAmt (s & sStakedVotes & bmMap)
    }

applyVote :: ModelSource -> [PermitProtected VoteParam] -> ModelT cep ()
applyVote mso = mapM_ acceptVote
  where
    acceptVote :: PermitProtected VoteParam -> ModelT cep ()
    acceptVote pp = do
      (voteParam, author) <- verifyPermitProtectedVote mso pp

      validFrom <- checkDelegate (pp & ppArgument & vFrom) (toAddress author)
      proposal <- checkIfProposalExist (voteParam & vProposalKey)
      ensureProposalVotingStage proposal
      submitVote proposal voteParam validFrom

applyFreeze :: ModelSource -> FreezeParam -> ModelT cep ()
applyFreeze mso (arg #amount -> param) = do
  let senderAddr = mso & msoSender
  let amt = param

  freezingUpdateFh (toAddress senderAddr) (toInteger amt)
  lockGovernanceTokens amt (toAddress senderAddr)
    >>= \frozenTotalSupply -> modifyStore $ \s -> pure $ s
            { sFrozenTotalSupply = frozenTotalSupply
            }


lockGovernanceTokens :: Natural -> Address -> ModelT cep Natural
lockGovernanceTokens tokens addr = do
  frozenTotalSupply <- getStore <&> sFrozenTotalSupply
  governanceToken <- getStore <&> sGovernanceToken
  selfAddr <- get <&> msSelfAddress
  let param = FA2.TransferItem
            { tiFrom = addr
            , tiTxs = [ FA2.TransferDestination
                { tdTo = selfAddr
                , tdTokenId = (governanceToken & gtTokenId)
                , tdAmount = tokens
                } ]
            }
  _ <- makeTransferOnToken [param] (governanceToken & gtAddress)
  pure (frozenTotalSupply + tokens)

unlockGovernanceTokens :: Natural -> Address -> ModelT cep Natural
unlockGovernanceTokens tokens addr = do
  frozenTotalSupply <- getStore <&> sFrozenTotalSupply
  governanceToken <- getStore <&> sGovernanceToken
  selfAddr <- get <&> msSelfAddress
  let param = FA2.TransferItem
            { tiFrom = selfAddr
            , tiTxs = [ FA2.TransferDestination
                { tdTo = addr
                , tdTokenId = (governanceToken & gtTokenId)
                , tdAmount = tokens
                } ]
            }
  _ <- makeTransferOnToken [param] (governanceToken & gtAddress)
  case frozenTotalSupply `minusNaturalMaybe` tokens of
    Just n -> pure n
    Nothing -> error "BAD_STATE: Unlocking more `frozenTotalSupply`."


applyUnfreeze :: ModelSource -> UnfreezeParam -> ModelT cep ()
applyUnfreeze mso (arg #amount -> amt) = do
  let senderAddr = mso & msoSender

  freezingUpdateFh (toAddress senderAddr) (negate $ fromIntegral amt)

  unlockGovernanceTokens amt (toAddress senderAddr)
    >>= \frozenTotalSupply -> modifyStore $ \s -> pure $ s
            { sFrozenTotalSupply = frozenTotalSupply }


isLevelReached :: Proposal -> Natural -> ModelT cep Bool
isLevelReached proposal target = do
  lvl <- msLevel <$> get
  pure (lvl >= (proposal & plStartLevel) + target)


handleProposalIsOver
  :: Proposal
  -> ModelT var ()
handleProposalIsOver proposal = do
  cond <- getStore <&> \s ->
            doTotalVoteMeetQuorumThreshold proposal s && ((proposal & plUpvotes) > (proposal & plDownvotes))
  unstakeProposerToken cond proposal
  -- Execute decision callback if the proposal is passed.
  when cond $ do
    store <- getStore
    (ops, newExtra, guardianMaybe) <-
      get <&> msDecisionCallback >>= \f -> f (DecisionCallbackInput' proposal (store & sExtra))

    case guardianMaybe of
      Just g -> modifyStore $ \s -> pure $ s { sGuardian = g }
      Nothing -> pure ()

    modifyStore $ \s -> pure $ s { sExtra = newExtra }
    mapM_ execOperation ops


flushEach :: Integer -> ModelT var Integer
flushEach n = do
  store <- getStore
  let (plistHead, plistNew) = plistPop (store & sOngoingProposalsDlist)
  case plistHead of
    Just firstKey -> do
      proposal <- checkIfProposalExist firstKey
      isExpired <- isLevelReached proposal =<< (getConfig <&> cProposalExpiredLevel)
      canBeFlushed <- isLevelReached proposal =<< (getConfig <&> cProposalFlushLevel)
      when isExpired $
        throwError EXPIRED_PROPOSAL

      if (n > 0 && canBeFlushed) then do
        handleProposalIsOver proposal
        modifyStore $ \s -> pure $ s { sOngoingProposalsDlist = plistNew}
        flushEach (n - 1)
      else
        pure n
    Nothing -> pure n


applyFlush :: ModelSource -> Natural -> ModelT cep ()
applyFlush _ param = do

  newN <- flushEach (toInteger param)

  if (newN == toInteger param) then throwError EMPTY_FLUSH
  else pure ()

applyDropProposal :: ModelSource -> ProposalKey -> ModelT cep ()
applyDropProposal mso proposalKey = do

  proposal <- checkIfProposalExist proposalKey
  proposalIsExpired <- isLevelReached proposal =<< (getConfig <&> cProposalExpiredLevel)

  store <- getStore
  if   (toAddress (mso & msoSender) == (proposal & plProposer))
    || (toAddress (mso & msoSender) == (store & sGuardian) && (mso & msoSender) /= (mso & msoSource))
    || proposalIsExpired
  then do
    unstakeProposerToken False proposal
    modifyStore $ \s ->
      pure $ s
        { sOngoingProposalsDlist = plistDelete proposalKey (s & sOngoingProposalsDlist)
        }
  else
    throwError DROP_PROPOSAL_CONDITION_NOT_MET

---------------------------------------------------------------
-- Delegate
---------------------------------------------------------------

updateDelegate :: ModelSource -> Set Delegate -> DelegateParam -> Set Delegate
updateDelegate mso delegates param =
  if param & dpEnable
    then Set.insert delegate delegates
    else Set.delete delegate delegates
  where
    delegate = Delegate { dOwner = toAddress $ mso & msoSender, dDelegate = param & dpDelegate }

applyUpdateDelegate :: ModelSource -> [DelegateParam] -> ModelT cep ()
applyUpdateDelegate mso params =
  modifyStore $ \s ->
    let delegates = Map.keysSet . bmMap . sDelegates $ s
        updatedDelegates = foldl' (updateDelegate mso) delegates params
    in
      pure $ s { sDelegates = BigMap Nothing $ Map.fromSet (const ()) updatedDelegates }

---------------------------------------------------------------
-- Unstake_vote
---------------------------------------------------------------

applyUnstakeVoteOne :: ModelSource -> ProposalKey -> ModelT cep ()
applyUnstakeVoteOne mso key = do
  store <- getStore

  -- Ensure proposal is already flushed or dropped.
  when (plistMem key (store & sOngoingProposalsDlist)) $
    throwError UNSTAKE_INVALID_PROPOSAL

  tokens <- case Map.lookup (toAddress $ mso & msoSender, key) (store & sStakedVotes & bmMap) of
          Just v -> pure v
          Nothing -> throwError VOTER_DOES_NOT_EXIST

  unstakeTk tokens 0 (toAddress $ mso & msoSender)

  modifyStore $ \s -> pure $ s
    { sStakedVotes = BigMap Nothing $ Map.delete (toAddress $ mso & msoSender, key) (s & sStakedVotes & bmMap)
    }

applyUnstakeVote :: ModelSource -> UnstakeVoteParam -> ModelT cep ()
applyUnstakeVote mso params =
  mapM_ (applyUnstakeVoteOne mso) params
