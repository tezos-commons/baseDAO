-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

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
  , proposalBoundedValue
  , proposerIsReturnedFeeAfterSucceeding
  , rejectProposal
  , validProposal
  , validProposalWithFixedFee
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data FailureReason = QuorumNotMet | Downvoted

vote :: Bool -> ProposalKey -> PermitProtected VoteParam
vote how key =
  NoPermit VoteParam
    { vVoteType = how
    , vVoteAmount = 1
    , vProposalKey = key
    }

downvote :: ProposalKey -> PermitProtected VoteParam
downvote = vote False

validProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetTotalSupplyFn m -> m ()
validProposal originateFn getTotalSupplyFn = do
  DaoOriginateData{..} <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  -- Check the token contract got a transfer call from
  -- baseDAO
  checkStorage (unTAddress dodTokenContract)
    (toVal [[FA2.TransferItem { tiFrom = dodOwner1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }] }]])

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
  checkTokenBalance frozenTokenId dodDao dodOwner1 110

  -- Check total supply
  totalSupply <- getTotalSupplyFn (unTAddress dodDao ) frozenTokenId
  totalSupply @== 210 -- initial = 0

validProposalWithFixedFee
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => GetTotalSupplyFn m -> m ()
validProposalWithFixedFee totalSupply = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 42))
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }
  let proposer = dodOwner1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 52)
  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  withSender proposer $ call dodDao (Call @"Propose") params
  checkTokenBalance frozenTokenId dodDao proposer 152

  totalSupply_ <- totalSupply (unTAddress dodDao) frozenTokenId
  totalSupply_ @== 252 -- initial = 0

proposerIsReturnedFeeAfterSucceeding
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
proposerIsReturnedFeeAfterSucceeding = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe
      (   (ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      >>- (ConfigDesc (FixedFee 42))
      )
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender voter $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 42)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  key1 <- createSampleProposal 1 proposer dodDao
  -- Advance one voting period to a voting stage.
  advanceLevel 60
  let vote_ =
        NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 10
          , vProposalKey = key1
          }
  withSender voter $
    call dodDao (Call @"Vote") [vote_]

  let expectedFrozen = 100 + 42 + 10
  checkTokenBalance frozenTokenId dodDao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  advanceLevel 60
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  checkTokenBalance frozenTokenId dodDao proposer 152

cannotProposeWithInsufficientTokens
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
cannotProposeWithInsufficientTokens = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 100))
  let proposer = dodOwner1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 52)
  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  let params = ProposeParams
        { ppFrozenToken = 1
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }
  withSender proposer $ call dodDao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao

rejectProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
rejectProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dodDao

nonUniqueProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonUniqueProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10
  _ <- createSampleProposal 1 dodOwner1 dodDao
  createSampleProposal 1 dodOwner1 dodDao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE dodDao

nonProposalPeriodProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonProposalPeriodProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance two voting periods to another voting stage.
  advanceLevel 20

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomErrorNoArg #nOT_PROPOSING_STAGE dodDao

burnsFeeOnFailure
  :: forall caps base m. (MonadNettest caps base m)
  => FailureReason -> m ()
burnsFeeOnFailure reason = do
  DaoOriginateData{..} <-
      originateLigoDaoWithConfigDesc dynRecUnsafe
        (   (ConfigDesc $ Period 60)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
        >>- (ConfigDesc $ FixedFee 42)
        )
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 42)

  withSender voter $
    call dodDao (Call @"Freeze") (#amount .! 1)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 61
  key1 <- createSampleProposal 1 proposer dodDao

  -- Advance one voting period to a voting stage.
  advanceLevel 60
  case reason of
    Downvoted -> do
      withSender voter $
        call dodDao (Call @"Vote") [downvote key1]
    QuorumNotMet -> return ()

  let expectedFrozen = 100 + 42 + 10
  checkTokenBalance frozenTokenId dodDao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  advanceLevel 61
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- Tokens frozen with the proposal are returned as unstaked (but still
  -- frozen), except for the fee and slash amount. The latter is zero in this
  -- case, so we expect 42 tokens to be burnt
  let expectedBurn = 42
  checkTokenBalance frozenTokenId dodDao proposer (100 + (52 - expectedBurn))

insufficientTokenProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> (Address -> m Int) -> m ()
insufficientTokenProposal originateFn getProposalAmountFn = do
  DaoOriginateData{..} <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao
  amt <- getProposalAmountFn (unTAddress dodDao)
  amt @== 0

insufficientTokenVote
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
insufficientTokenVote originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig
  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 100)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 51
            , vProposalKey = key1
           }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 50
            , vProposalKey = key1
            }
        ]
  -- Advance one voting period to a voting stage.
  advanceLevel 10

  withSender dodOwner2 $ call dodDao (Call @"Vote") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS dodDao

dropProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
dropProposal originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (configWithRejectedProposal
       >>- (ConfigDesc (Period 20))
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
       >>- (ConfigDesc (mkQuorumThreshold 1 50))
       >>- (ConfigDesc (Period 20))
      )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceLevel 20

  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceLevel 20
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceLevel 20

  key3 <- createSampleProposal 3 dodOwner1 dodDao

  -- `guardian` contract can drop any proposal.
  withSender dodOwner2 $ do
    call dodGuardian CallDefault (unTAddress dodDao, key1)

  -- `key2` is not yet expired since it has to be more than 60 seconds
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2
      & expectCustomErrorNoArg #dROP_PROPOSAL_CONDITION_NOT_MET dodDao

  advanceLevel 21
  -- `key2` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2

  -- `key3` is not yet expired
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key3
      & expectCustomErrorNoArg #dROP_PROPOSAL_CONDITION_NOT_MET dodDao

  -- proposers can delete their proposal
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key3

  -- 30 tokens are frozen in total, but only 15 tokens are returned after drop_proposal
  checkTokenBalance frozenTokenId dodDao dodOwner1 115

proposalBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
proposalBoundedValue originateFn = do
  DaoOriginateData{..} <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts{ cmMaxProposals = Just 1 }
    )

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        }

  withSender dodOwner1 $ do
    call dodDao (Call @"Propose") params
    call dodDao (Call @"Propose") params
      & expectCustomErrorNoArg #mAX_PROPOSALS_REACHED dodDao

