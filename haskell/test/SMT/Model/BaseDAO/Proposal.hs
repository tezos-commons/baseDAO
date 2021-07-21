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
  ) where

import Universum

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Natural

import Lorentz hiding (not, cast, get, take, or)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Michelson.Typed as T

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
        p <- Map.lookup key (store & sProposals & T.unBigMap)
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
  if (author /= from) && not (Map.member key (store & sDelegates & T.unBigMap)) then
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
  case Map.member proposalKey (store & sProposals & T.unBigMap) of
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
        , plVoters = mempty
        , plQuorumThreshold = store & sQuorumThresholdAtCycle & qaQuorumThreshold
        }
  modifyStore $ \s -> pure $ s
    { sProposals = BigMap $ Map.insert proposalKey proposal (T.unBigMap (s & sProposals))
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

unfreezeProposerAndVoterToken
  :: Bool -> Proposal -> ModelT ()
unfreezeProposerAndVoterToken isAccepted proposal = do
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

  mapM_ doUnfreeze (Map.toList (proposal & plVoters))
  where
    doUnfreeze :: ((Address, Bool), Natural) -> ModelT ()
    doUnfreeze ((addr, _), voteAmount) = do
      unstakeTk voteAmount 0 addr

deleteProposal :: Natural -> ProposalKey -> ModelT ()
deleteProposal pLevel proposalKey = modifyStore $ \s ->
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

checkVoteLimitReached :: Proposal -> ModelT ()
checkVoteLimitReached pl = do
  maxVoters <- getConfig <&> cMaxVoters
  when (maxVoters <= (fromIntegral $ length (pl & plVoters))) $
    throwError MAX_VOTERS_REACHED

ensureProposalVotingStage :: Proposal -> ModelT ()
ensureProposalVotingStage proposal = do
  currentStage <- getCurrentStageNum
  unless (currentStage == (proposal & plVotingStageNum)) $
    throwError VOTING_STAGE_OVER

submitVote :: Proposal -> VoteParam -> Address -> ModelT ()
submitVote proposal voteParam author = do
  let proposalKey = voteParam & vProposalKey
      proposal_ =
        let mapKey = (author, voteParam & vVoteType)
            newVotes = case Map.lookup mapKey (proposal & plVoters) of
              Just votes -> votes + (voteParam & vVoteAmount)
              Nothing -> voteParam & vVoteAmount
        in proposal { plVoters = Map.insert mapKey newVotes (proposal & plVoters) }

      updatedProposal = if (voteParam & vVoteType)
                        then proposal_ { plUpvotes = (proposal_ & plUpvotes) + (voteParam & vVoteAmount)}
                        else proposal_ { plDownvotes = (proposal_ & plDownvotes) + (voteParam & vVoteAmount)}
  stakeTk (voteParam & vVoteAmount) author
  modifyStore $ \s -> pure $ s { sProposals = BigMap $ Map.insert proposalKey updatedProposal (s & sProposals & T.unBigMap)}


applyVote :: ModelSource -> [PermitProtected VoteParam] -> ModelT ()
applyVote mso = mapM_ acceptVote
  where
    acceptVote :: PermitProtected VoteParam -> ModelT ()
    acceptVote pp = do
      (voteParam, author) <- verifyPermitProtectedVote mso pp

      validFrom <- checkDelegate (pp & ppArgument & vFrom) author
      proposal <- checkIfProposalExist (voteParam & vProposalKey)
      checkVoteLimitReached proposal
      ensureProposalVotingStage proposal
      submitVote proposal voteParam validFrom

applyFreeze :: ModelSource -> FreezeParam -> ModelT ()
applyFreeze mso param = do
  let senderAddr = mso & msoSender
  let amt = arg #amount param

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
applyUnfreeze mso param = do
  let senderAddr = mso & msoSender
  let amt = arg #amount param

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

    unfreezeProposerAndVoterToken cond proposal
    when cond $ do
      store <- getStore
      (ops, newExtra, guardianMaybe) <-
        get <&> msDecisionLambda >>= \f -> f (DecisionLambdaInput proposal (store & sExtra))

      case guardianMaybe of
        Just g -> modifyStore $ \s -> pure $ s { sGuardian = g }
        Nothing -> pure ()

      modifyStore $ \s -> pure $ s { sExtra = newExtra }
      mapM_ execOperation ops

    deleteProposal proposalLvl proposalKey
    pure True
  else
    pure False

applyFlush :: ModelSource -> Natural -> ModelT ()
applyFlush _ param = do
  when (param == 0) $
    throwError BAD_ENTRYPOINT_PARAMETER

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
    unfreezeProposerAndVoterToken False proposal
    deleteProposal (proposal & plStartLevel) proposalKey
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
    let delegates = Map.keysSet . T.unBigMap . sDelegates $ s
        updatedDelegates = foldl' (updateDelegate mso) delegates params
    in
      pure $ s { sDelegates = BigMap $ Map.fromSet (const ()) updatedDelegates }

