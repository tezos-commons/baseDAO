-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE ApplicativeDo #-}
-- | Contains tests on @propose@ entrypoint logic for testing the Ligo contract.
module Test.Ligo.BaseDAO.Proposal.Propose
  ( FailureReason(..)
  , burnsFeeOnFailure
  , cannotProposeWithInsufficientTokens
  , dropProposal
  , insufficientTokenProposal
  , insufficientTokenVote
  , nonProposalPeriodProposal
  , nonUniqueProposal
  , nonUniqueProposalEvenAfterDrop
  , proposerIsReturnedFeeAfterSucceeding
  , validProposal
  , validProposalWithFixedFee
  , unstakesTokensForMultipleVotes
  , unstakeVote
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data FailureReason = QuorumNotMet | Downvoted

vote :: Bool -> Address -> ProposalKey -> PermitProtected VoteParam
vote how addr key =
  NoPermit VoteParam
    { vVoteType = how
    , vVoteAmount = 1
    , vProposalKey = key
    , vFrom = addr
    }

downvote :: Address -> ProposalKey -> PermitProtected VoteParam
downvote = vote False

validProposal
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
validProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)
  -- Check the token contract got a transfer call from
  -- baseDAO
  storage <- getStorage @[[FA2.TransferItem]] (unTAddress dodTokenContract)
  assert (storage ==
    ([[FA2.TransferItem { tiFrom = dodOwner1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }] }]]))
    "Unexpected Transfers"

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $ call dodDao (Call @"Propose") params

  -- Check balance
  supply <- getFrozenTotalSupply dodDao
  supply @== 10 -- initial = 0

  -- Check freeze history
  fh <- getFreezeHistory dodDao dodOwner1
  fh @== Just AddressFreezeHistory
    { fhCurrentStageNum = 1
    , fhStaked = 10
    , fhCurrentUnstaked = 0
    , fhPastUnstaked = 0
    }

validProposalWithFixedFee
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => m ()
validProposalWithFixedFee = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc @'Base () (ConfigDesc (FixedFee 42)) defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }
  let proposer = dodOwner1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 52)
  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender proposer $ call dodDao (Call @"Propose") params

  supply <- getFrozenTotalSupply dodDao
  supply @== 52
  fh <- getFreezeHistory dodDao dodOwner1
  fh @== Just AddressFreezeHistory
    { fhCurrentStageNum = 1
    , fhStaked = 52
    , fhCurrentUnstaked = 0
    , fhPastUnstaked = 0
    }

proposerIsReturnedFeeAfterSucceeding
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => m ()
proposerIsReturnedFeeAfterSucceeding = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc @'Base ()
      ((ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      >>- (ConfigDesc (FixedFee 42))
      ) defaultQuorumThreshold
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender voter $
    call dodDao (Call @"Freeze") (#amount :! 20)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 42)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 proposer dodDao
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  let vote_ =
        NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 10
          , vProposalKey = key1
          , vFrom = voter
          }
  withSender voter $
    call dodDao (Call @"Vote") [vote_]

  let expectedFrozen = 42 + 10
  checkBalance dodDao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  checkBalance dodDao proposer 52

cannotProposeWithInsufficientTokens
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => m ()
cannotProposeWithInsufficientTokens = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc @'Base () (ConfigDesc (FixedFee 100)) defaultQuorumThreshold
  let proposer = dodOwner1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 52)
  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  let params = ProposeParams
        { ppFrozenToken = 1
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }
  withSender proposer $ call dodDao (Call @"Propose") params
    & expectFailedWith notEnoughFrozenTokens

nonUniqueProposal
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
nonUniqueProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)
  _ <- createSampleProposal 1 dodOwner1 dodDao
  createSampleProposal 1 dodOwner1 dodDao
    & expectFailedWith proposalNotUnique

nonUniqueProposalEvenAfterDrop
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
nonUniqueProposalEvenAfterDrop originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  withSender dodOwner1 $ call dodDao (Call @"Drop_proposal") key1
  createSampleProposal 1 dodOwner1 dodDao
    & expectFailedWith proposalNotUnique

nonProposalPeriodProposal
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
nonProposalPeriodProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  -- Advance two voting periods to another voting stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + 2 * dodPeriod)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectFailedWith notProposingStage

burnsFeeOnFailure
  :: forall caps m. (MonadCleveland caps m)
  => FailureReason -> m ()
burnsFeeOnFailure reason = do
  DaoOriginateData{..} <-
      originateLigoDaoWithConfigDesc @'Base ()
        (   (ConfigDesc $ Period 60)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
        >>- (ConfigDesc $ FixedFee 42)
        ) defaultQuorumThreshold
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 42)

  withSender voter $
    call dodDao (Call @"Freeze") (#amount :! 1)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 10)

  startLevel <- getOriginationLevel dodDao

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 proposer dodDao

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  case reason of
    Downvoted -> do
      withSender voter $
        call dodDao (Call @"Vote") [downvote voter key1]
    QuorumNotMet -> return ()

  let expectedFrozen = 42 + 10
  checkBalance dodDao proposer expectedFrozen

  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- Tokens frozen with the proposal are returned as unstaked (but still
  -- frozen), except for the fee and slash amount. The latter is zero in this
  -- case, so we expect 42 tokens to be burnt
  let expectedBurn = 42
  checkBalance dodDao proposer (52 - expectedBurn - 1)

unstakesTokensForMultipleVotes
  :: forall caps m. (MonadCleveland caps m)
  => m ()
unstakesTokensForMultipleVotes = do
  DaoOriginateData{..} <-
      originateLigoDaoWithConfigDesc @'Base ()
        (   (ConfigDesc $ Period 60)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
        >>- (ConfigDesc $ FixedFee 42)
        ) defaultQuorumThreshold
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount :! 52)

  withSender voter $
    call dodDao (Call @"Freeze") (#amount :! 30)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 proposer dodDao

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  let vote_ typ =
        NoPermit VoteParam
          { vVoteType = typ
          , vVoteAmount = 10
          , vProposalKey = key1
          , vFrom = voter
          }

  withSender voter . inBatch $ do
    call dodDao (Call @"Vote") [vote_ True]
    call dodDao (Call @"Vote") [vote_ False]
    return ()

  fh <- getFreezeHistory dodDao voter
  let expected = Just (AddressFreezeHistory
        { fhCurrentUnstaked = 0
        , fhPastUnstaked = 10
        , fhCurrentStageNum = 2
        , fhStaked = 20
        })
  assert (fh == expected) "Unexpected freeze history after voting"

  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  withSender voter $ call dodDao (Call @"Unstake_vote") [key1]

  fh_after_flush <- getFreezeHistory dodDao voter
  let expected_after_flush = Just (AddressFreezeHistory
        { fhCurrentUnstaked = 0
        , fhPastUnstaked = 30
        , fhCurrentStageNum = 3
        , fhStaked = 0
        })
  assert (fh_after_flush == expected_after_flush) "Unexpected freeze history after flush"

insufficientTokenProposal
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
insufficientTokenProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectFailedWith notEnoughFrozenTokens

insufficientTokenVote
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
insufficientTokenVote originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 100)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 51
            , vProposalKey = key1
            , vFrom = dodOwner2
           }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 50
            , vProposalKey = key1
            , vFrom = dodOwner2
            }
        ]
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)

  withSender dodOwner2 $ call dodDao (Call @"Vote") params
    & expectFailedWith notEnoughFrozenTokens

dropProposal
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m) -> m ()
dropProposal originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (testConfig
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 50 })
      ) (mkQuorumThreshold 1 50)

  startLevel <- getOriginationLevel' @'Base dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        , vFrom = dodOwner2
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + 3*dodPeriod)

  key3 <- createSampleProposal 3 dodOwner1 dodDao
  proposalStart2 <- getProposalStartLevel' @'Base dodDao key2
  proposalStart3 <- getProposalStartLevel' @'Base dodDao key3

  -- `guardian` contract can drop any proposal.
  withSender dodOwner2 $ do
    call dodGuardian CallDefault (unTAddress dodDao, key1)

  -- `key2` is not yet expired since it has to be more than 60
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2
      & expectFailedWith dropProposalConditionNotMet

  advanceToLevel (proposalStart2 + 50)
  -- `key2` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2

  -- `key3` is not yet expired
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key3
      & expectFailedWith dropProposalConditionNotMet

  advanceToLevel (proposalStart3 + 50)
  -- proposers can delete their proposal
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key3

  -- calling drop proposal again results in an error
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key3
      & expectFailedWith proposalNotExist

  -- 30 tokens are frozen in total, but only 27 tokens are returned after drop_proposal
  -- because 3 proposals are droped, buring one token for each. (trivialDOA is supposed to
  -- burn a single token when proposal is rejected).
  checkBalance' @'Base dodDao dodOwner1 27

unstakeVote
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
unstakeVote originateFn = do
  let flushLevel = 20
  DaoOriginateData{..}
    <- originateFn (testConfig
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just flushLevel })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 50 })
        ) defaultQuorumThreshold

  -- [Voting]
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 10)

  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  -- [Proposing]
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  advanceLevel 1
  key2 <- createSampleProposal 2 dodOwner1 dodDao

  let vote' key = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 5
        , vProposalKey = key
        }

  -- Advance to a level where vote can be called
  advanceToLevel (startLevel + 2 * dodPeriod)

  -- [Voting]
  withSender dodOwner2 . inBatch $ do
      call dodDao (Call @"Vote") [vote' key1]
      call dodDao (Call @"Vote") [vote' key2]
      pure ()

  -- Advance to a level where flush can be called
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + (flushLevel + 1))

  -- [Proposing]
  withSender dodAdmin $ call dodDao (Call @"Flush") 1

  -- Freeze history before `Unstake_vote`
  fhOwner2 <- getFreezeHistory dodDao dodOwner2
  (fhOwner2 <&> fhStaked) @== Just 10
  (fhOwner2 <&> fhPastUnstaked) @== Just 0
  (fhOwner2 <&> fhCurrentUnstaked) @== Just 0

  -- Call succeeds.
  withSender dodOwner2 $ call dodDao (Call @"Unstake_vote") [key1]

  -- Freeze history after `Unstake_vote`
  fhOwner2_ <- getFreezeHistory dodDao dodOwner2
  (fhOwner2_ <&> fhStaked) @== Just 5 -- Still staked since `key2` is not yet flush.
  (fhOwner2_ <&> fhPastUnstaked) @== Just 5
  (fhOwner2_ <&> fhCurrentUnstaked) @== Just 0

  -- Trigger error since the tokens are already unstaked.
  withSender dodOwner2 $ call dodDao (Call @"Unstake_vote") [key1]
    & expectFailedWith voterDoesNotExist

  -- Proposal key2 is not yet flush.
  withSender dodOwner2 $ call dodDao (Call @"Unstake_vote") [key2]
    & expectFailedWith unstakeInvalidProposal

  -- owner 1 does not vote on the proposal
  withSender dodOwner1 $ call dodDao (Call @"Unstake_vote") [key1]
    & expectFailedWith voterDoesNotExist

  --------------------------------------
  -- Unstake_vote on dropped proposal
  --------------------------------------
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key2

  advanceToLevel (startLevel + 4 * dodPeriod)
  -- Call succeeds.
  withSender dodOwner2 $ call dodDao (Call @"Unstake_vote") [key2]

  -- Freeze history after `Unstake_vote`
  fhOwner2__ <- getFreezeHistory dodDao dodOwner2
  (fhOwner2__ <&> fhStaked) @== Just 0
  (fhOwner2__ <&> fhPastUnstaked) @== Just 10
  (fhOwner2__ <&> fhCurrentUnstaked) @== Just 0
