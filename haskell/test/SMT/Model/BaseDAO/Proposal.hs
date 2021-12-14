-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

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
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Natural

import Lorentz hiding (cast, div, get, not, or, take)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Michelson.Typed.Haskell.Value (BigMap(..))
import Morley.Util.Named

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Permit
import SMT.Model.BaseDAO.Proposal.FreezeHistory
import SMT.Model.BaseDAO.Proposal.QuorumThreshold
import SMT.Model.BaseDAO.Token
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

checkIfProposalExist
  :: ProposalKey -> ModelT Proposal
checkIfProposalExist key = do
  store <- getStore

  let resultMaybe = do
        p <- Map.lookup key (store & sProposals & bmMap)
        if Set.member (p & plStartLevel, key) (store & sProposalKeyListSortByDate) then
          Just p
        else Nothing

  case resultMaybe of
    Just p -> pure p
    Nothing -> throwError PROPOSAL_NOT_EXIST


checkDelegate
  :: Address -> Address -> ModelT Address
checkDelegate from author  = do
  store <- getStore
  let key = Delegate {dOwner = from, dDelegate = author}
  if (author /= from) && not (Map.member key (store & sDelegates & bmMap)) then
    throwError NOT_DELEGATE
  else pure from

checkProposalLimitReached :: ModelT ()
checkProposalLimitReached = do
  proposalKeyListSortByDate <- getStore <&> sProposalKeyListSortByDate
  maxProposal <- getConfig <&> cMaxProposals
  when (maxProposal <= (fromIntegral $ length proposalKeyListSortByDate)) $
    throwError MAX_PROPOSALS_REACHED

stakeTk :: Natural -> Address -> ModelT ()
stakeTk tokenAmount addr = do
  stakingUpdateFh addr $ fromIntegral tokenAmount
  modifyStore $ \s -> do
    let newCycleStaked = (s & sQuorumThresholdAtCycle & qaStaked) + tokenAmount
    pure $ s
      { sQuorumThresholdAtCycle = (s & sQuorumThresholdAtCycle) { qaStaked = newCycleStaked}
      }

ensureProposingStage :: Natural -> ModelT ()
ensureProposingStage stageNum = do
  unless (stageNum `mod` 2 == 1) $
    throwError NOT_PROPOSING_STAGE


ensureProposalIsUnique :: ProposeParams -> ModelT ProposalKey
ensureProposalIsUnique params = do
  store <- getStore
  let proposalKey = makeProposalKey params
  case Map.member proposalKey (store & sProposals & bmMap) of
    True -> throwError PROPOSAL_NOT_UNIQUE
    False -> pure proposalKey


addProposal :: ProposeParams -> ModelT ()
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
    , sProposalKeyListSortByDate = Set.insert (lvl, proposalKey) (s & sProposalKeyListSortByDate)
    }

unstakeTk :: Natural -> Natural -> Address -> ModelT ()
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


doTotalVoteMeetQuorumThreshold :: Proposal -> Storage -> Bool
doTotalVoteMeetQuorumThreshold proposal store =
  let votesPlaced = (proposal & plUpvotes) + (proposal & plDownvotes)
      totalSupply = store & sFrozenTotalSupply
      reachedQuorum = QuorumThreshold $ (votesPlaced * fromIntegral fractionDenominator) `div` totalSupply
  in  (reachedQuorum >= (proposal & plQuorumThreshold))

unstakeProposerToken
  :: Bool -> Proposal -> ModelT ()
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


removeFromProposalSortByLevel :: Natural -> ProposalKey -> ModelT ()
removeFromProposalSortByLevel pLevel proposalKey = modifyStore $ \s ->
  pure $ s { sProposalKeyListSortByDate = Set.delete (pLevel, proposalKey) (s & sProposalKeyListSortByDate)}

applyPropose :: ModelSource -> ProposeParams -> ModelT ()
applyPropose mso param@ProposeParams{..} = do

  config <- getConfig
  let FixedFee fixedFee = config & cFixedProposalFee

  validFrom <- checkDelegate ppFrom (mso & msoSender)

  store <- getStore
  get <&> msProposalCheck >>= \f -> f (param, (store & sExtra))
  checkProposalLimitReached

  let amountToFreeze = ppFrozenToken + fixedFee
  currentStage <- getCurrentStageNum

  updateQuorum currentStage
  stakeTk amountToFreeze validFrom
  addProposal param

ensureProposalVotingStage :: Proposal -> ModelT ()
ensureProposalVotingStage proposal = do
  currentStage <- getCurrentStageNum
  unless (currentStage == (proposal & plVotingStageNum)) $
    throwError VOTING_STAGE_OVER

submitVote :: Proposal -> VoteParam -> Address -> ModelT ()
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

applyVote :: ModelSource -> [PermitProtected VoteParam] -> ModelT ()
applyVote mso = mapM_ acceptVote
  where
    acceptVote :: PermitProtected VoteParam -> ModelT ()
    acceptVote pp = do
      (voteParam, author) <- verifyPermitProtectedVote mso pp

      validFrom <- checkDelegate (pp & ppArgument & vFrom) author
      proposal <- checkIfProposalExist (voteParam & vProposalKey)
      ensureProposalVotingStage proposal
      submitVote proposal voteParam validFrom

applyFreeze :: ModelSource -> FreezeParam -> ModelT ()
applyFreeze mso (N param) = do
  let senderAddr = mso & msoSender
  let amt = param

  freezingUpdateFh senderAddr (toInteger amt)
  lockGovernanceTokens amt senderAddr
    >>= \frozenTotalSupply -> modifyStore $ \s -> pure $ s
            { sFrozenTotalSupply = frozenTotalSupply
            }


lockGovernanceTokens :: Natural -> Address -> ModelT Natural
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

unlockGovernanceTokens :: Natural -> Address -> ModelT Natural
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


applyUnfreeze :: ModelSource -> UnfreezeParam -> ModelT ()
applyUnfreeze mso (N amt) = do
  let senderAddr = mso & msoSender

  freezingUpdateFh senderAddr (negate $ fromIntegral amt)

  unlockGovernanceTokens amt senderAddr
    >>= \frozenTotalSupply -> modifyStore $ \s -> pure $ s
            { sFrozenTotalSupply = frozenTotalSupply }


isLevelReached :: Proposal -> Natural -> ModelT Bool
isLevelReached proposal target = do
  lvl <- get <&> msLevel
  pure (lvl >= (proposal & plStartLevel) + target)

handleProposalIsOver :: (Natural, ProposalKey) -> ModelT Bool
handleProposalIsOver (proposalLvl, proposalKey) = do
  proposal <- checkIfProposalExist proposalKey

  isExpired <- isLevelReached proposal =<< (getConfig <&> cProposalExpiredLevel)
  canBeFlushed <- isLevelReached proposal =<< (getConfig <&> cProposalFlushLevel)
  if isExpired then
    throwError EXPIRED_PROPOSAL
  else if canBeFlushed then do
    cond <- getStore <&> \s ->
              doTotalVoteMeetQuorumThreshold proposal s && ((proposal & plUpvotes) > (proposal & plDownvotes))

    unstakeProposerToken cond proposal
    when cond $ do
      store <- getStore
      (ops, newExtra, guardianMaybe) <-
        get <&> msDecisionLambda >>= \f -> f (DecisionLambdaInput proposal (store & sExtra))

      case guardianMaybe of
        Just g -> modifyStore $ \s -> pure $ s { sGuardian = g }
        Nothing -> pure ()

      modifyStore $ \s -> pure $ s { sExtra = newExtra }
      mapM_ execOperation ops

    removeFromProposalSortByLevel proposalLvl proposalKey
    pure True
  else
    pure False

applyFlush :: ModelSource -> Natural -> ModelT ()
applyFlush _ param = do
  when (param == 0) $
    throwError EMPTY_FLUSH

  anyFlushedProposal <-
    getStore
      <&> (take (fromIntegral param) . Set.toAscList . sProposalKeyListSortByDate)
      >>= mapM handleProposalIsOver
      <&> or

  unless anyFlushedProposal $ throwError EMPTY_FLUSH

applyDropProposal :: ModelSource -> ProposalKey -> ModelT ()
applyDropProposal mso proposalKey = do

  proposal <- checkIfProposalExist proposalKey
  proposalIsExpired <- isLevelReached proposal =<< (getConfig <&> cProposalExpiredLevel)

  store <- getStore
  if   ((mso & msoSender) == (proposal & plProposer))
    || ((mso & msoSender) == (store & sGuardian) && (mso & msoSender) /= (mso & msoSource))
    || proposalIsExpired
  then do
    unstakeProposerToken False proposal
    removeFromProposalSortByLevel (proposal & plStartLevel) proposalKey
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
    delegate = Delegate { dOwner = mso & msoSender, dDelegate = param & dpDelegate }

applyUpdateDelegate :: ModelSource -> [DelegateParam] -> ModelT ()
applyUpdateDelegate mso params =
  modifyStore $ \s ->
    let delegates = Map.keysSet . bmMap . sDelegates $ s
        updatedDelegates = foldl' (updateDelegate mso) delegates params
    in
      pure $ s { sDelegates = BigMap Nothing $ Map.fromSet (const ()) updatedDelegates }

---------------------------------------------------------------
-- Unstake_vote
---------------------------------------------------------------

applyUnstakeVoteOne :: ModelSource -> ProposalKey -> ModelT ()
applyUnstakeVoteOne mso key = do
  store <- getStore

  -- Ensure proposal is already flushed or dropped.
  case Map.lookup key (store & sProposals & bmMap) of
    Just p ->
      if Set.member (p & plStartLevel, key) (store & sProposalKeyListSortByDate) then
        throwError UNSTAKE_INVALID_PROPOSAL
      else pure ()
    Nothing -> throwError PROPOSAL_NOT_EXIST

  tokens <- case Map.lookup (mso & msoSender, key) (store & sStakedVotes & bmMap) of
          Just v -> pure v
          Nothing -> throwError VOTER_DOES_NOT_EXIST

  unstakeTk tokens 0 (mso & msoSender)

  modifyStore $ \s -> pure $ s
    { sStakedVotes = BigMap Nothing $ Map.delete (mso & msoSender, key) (s & sStakedVotes & bmMap)
    }

  pure ()

applyUnstakeVote :: ModelSource -> UnstakeVoteParam -> ModelT ()
applyUnstakeVote mso params =
  mapM_ (applyUnstakeVoteOne mso) params
