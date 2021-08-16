-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

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
  , proposalBoundedValue
  , proposerIsReturnedFeeAfterSucceeding
  , rejectProposal
  , validProposal
  , validProposalWithFixedFee
  , unstakesTokensForMultipleVotes
  , proposalCreationUnderStorageLimit
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Client.TezosClient (Alias(..))
import Morley.Nettest
import Morley.Nettest.Caps
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config
import Tezos.Address (parseAddress)

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

type instance AsRPC FA2.TransferItem = FA2.TransferItem

validProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
validProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)
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
  fh @== Just (AddressFreezeHistory 0 0 1 10)

validProposalWithFixedFee
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
validProposalWithFixedFee = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 42)) defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }
  let proposer = dodOwner1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 52)
  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender proposer $ call dodDao (Call @"Propose") params

  supply <- getFrozenTotalSupply dodDao
  supply @== 52
  fh <- getFreezeHistory dodDao dodOwner1
  fh @== Just (AddressFreezeHistory 0 0 1 52)

proposerIsReturnedFeeAfterSucceeding
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
proposerIsReturnedFeeAfterSucceeding = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe
      ((ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      >>- (ConfigDesc (FixedFee 42))
      ) defaultQuorumThreshold
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender voter $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 42)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 10)

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
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
cannotProposeWithInsufficientTokens = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc dynRecUnsafe (ConfigDesc (FixedFee 100)) defaultQuorumThreshold
  let proposer = dodOwner1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 52)
  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  let params = ProposeParams
        { ppFrozenToken = 1
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }
  withSender proposer $ call dodDao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

rejectProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
rejectProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  (withSender dodOwner1 $ call dodDao (Call @"Propose") params)
    & expectCustomError #fAIL_PROPOSAL_CHECK tooSmallXtzErrMsg

nonUniqueProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonUniqueProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)
  _ <- createSampleProposal 1 dodOwner1 dodDao
  createSampleProposal 1 dodOwner1 dodDao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE

nonUniqueProposalEvenAfterDrop
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonUniqueProposalEvenAfterDrop originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  withSender dodOwner1 $ call dodDao (Call @"Drop_proposal") key1
  createSampleProposal 1 dodOwner1 dodDao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE

nonProposalPeriodProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonProposalPeriodProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance two voting periods to another voting stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + 2 * dodPeriod)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomErrorNoArg #nOT_PROPOSING_STAGE

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
        ) defaultQuorumThreshold
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 42)

  withSender voter $
    call dodDao (Call @"Freeze") (#amount .! 1)

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 10)

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
  checkBalance dodDao proposer (52 - expectedBurn)

unstakesTokensForMultipleVotes
  :: forall caps base m. (MonadNettest caps base m)
  => m ()
unstakesTokensForMultipleVotes = do
  DaoOriginateData{..} <-
      originateLigoDaoWithConfigDesc dynRecUnsafe
        (   (ConfigDesc $ Period 60)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
        >>- (ConfigDesc $ FixedFee 42)
        ) defaultQuorumThreshold
  let proposer = dodOwner1
  let voter = dodOwner2

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 52)

  withSender voter $
    call dodDao (Call @"Freeze") (#amount .! 30)

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
  fh_after_flush <- getFreezeHistory dodDao voter
  let expected_after_flush = Just (AddressFreezeHistory
        { fhCurrentUnstaked = 0
        , fhPastUnstaked = 30
        , fhCurrentStageNum = 3
        , fhStaked = 0
        })
  assert (fh_after_flush == expected_after_flush) "Unexpected freeze history after flush"

insufficientTokenProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> (Address -> m Int) -> m ()
insufficientTokenProposal originateFn getProposalAmountFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let params = ProposeParams
        { ppFrozenToken = 101
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS
  amt <- getProposalAmountFn (unTAddress dodDao)
  amt @== 0

insufficientTokenVote
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
insufficientTokenVote originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 100)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

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
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

dropProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
dropProposal originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (testConfig
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 20 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 30 })
      ) (mkQuorumThreshold 1 50)

  startLevel <- getOriginationLevel dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

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
  proposalStart2 <- getProposalStartLevel dodDao key2
  proposalStart3 <- getProposalStartLevel dodDao key3

  -- `guardian` contract can drop any proposal.
  withSender dodOwner2 $ do
    call dodGuardian CallDefault (unTAddress dodDao, key1)

  -- `key2` is not yet expired since it has to be more than 60
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2
      & expectCustomErrorNoArg #dROP_PROPOSAL_CONDITION_NOT_MET

  advanceToLevel (proposalStart2 + 30)
  -- `key2` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key2

  -- `key3` is not yet expired
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key3
      & expectCustomErrorNoArg #dROP_PROPOSAL_CONDITION_NOT_MET

  advanceToLevel (proposalStart3 + 30)
  -- proposers can delete their proposal
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key3

  -- calling drop proposal again results in an error
  withSender dodOwner1 $ do
    call dodDao (Call @"Drop_proposal") key3
      & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

  -- 30 tokens are frozen in total, but only 15 tokens are returned after drop_proposal
  checkBalance dodDao dodOwner1 15

proposalBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
proposalBoundedValue originateFn = do
  DaoOriginateData{..} <- originateFn
    ( testConfig >>-
      ConfigDesc configConsts{ cmMaxProposals = Just 1 }
    ) defaultQuorumThreshold

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer 1
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $ do
    call dodDao (Call @"Propose") params
    call dodDao (Call @"Propose") params
      & expectCustomErrorNoArg #mAX_PROPOSALS_REACHED

proposalCreationUnderStorageLimit
  :: forall caps base m.
    ( MonadNettest caps base m
    , HasCallStack
    )
  => (ConfigDesc Config -> OriginateFn m) -> m ()
proposalCreationUnderStorageLimit originateFn = do

  addr2 <- resolveAddress $ Alias "addr2"
  dodOwner1 <- resolveAddress $ Alias "owner1"
  let dodDao = case parseAddress "KT1G8AzguRNXATWgbzzQBw9MHAaiKwBRijwa" of
        Right x -> (TAddress x :: TAddress Parameter)
        _ -> error "contract address lookup failed"
  runIO $ putTextLn $ show addr2
  let dodPeriod = 100
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw (1 :: Natural)
        , ppFrom = dodOwner1
        }

  let voteParam = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = makeProposalKey params
        , vFrom = addr2
        }

  originLevel <- getOriginationLevel dodDao
  nowLevel <- getLevel
  runIO $ putTextLn $ ("Level= " <> (show nowLevel))

  let periodNum = div (nowLevel - originLevel) dodPeriod
  runIO $ putTextLn $ show ("In level", originLevel, nowLevel, periodNum)

  -- when (mod periodNum 2 == 1) $ do
  --   let waitTill = originLevel + (periodNum + 1)*dodPeriod
  --   runIO $ putTextLn $ show ("In proposal period, waiting till", waitTill)
  --   advanceToLevel waitTill

  withSender addr2 $
    call dodDao (Call @"Freeze") (#amount .! 1)
  -- withSender addr2 $ call dodDao (Call @"Vote") [voteParam]

  -- Test
  {-# runIO $ putTextLn ("Originating...")
  DaoOriginateData {..} <- originateFn ((ConfigDesc $ configConsts { cmProposalExpiredTime = Just 1000000 }) >>- (ConfigDesc $ Period 100) >>- testConfig) defaultQuorumThreshold

  let totalTokens = 100000
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! totalTokens)

  addresses <- mapM id $ (\(s :: Text) -> do
    runIO $ putTextLn ("Creating address:" <> show s)
    na <- newAddress $ fromString $ toString s
    transfer $ TransferData na (toMutez 5000000) DefEpName ()
    pure na
    ) <$> ((\s -> "addr" <> (show s)) <$> [1..1000])

  mapM_ id $ ((\s -> do
    runIO $ putTextLn ("Freezing for " <> show s)
    withSender s $
      call dodDao (Call @"Freeze") (#amount .! 1)
    ) <$> addresses)

  nowLevel <- getLevel
  advanceToLevel (nowLevel + dodPeriod + 5)


  originLevel <- getOriginationLevel dodDao
  nowLevel <- getLevel
  runIO $ putTextLn $ ("Level= " <> (show nowLevel))

  let periodNum = div (nowLevel - originLevel) dodPeriod
  when (mod periodNum 2 == 0) $ do
    let waitTill = originLevel + (periodNum + 1)*dodPeriod
    runIO $ putTextLn $ show ("Not proposal period, waiting till", waitTill)
    advanceToLevel waitTill

  runIO $ putTextLn $ ("Creating proposal")

  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw (1 :: Natural)
        , ppFrom = dodOwner1
        }

  withSender dodOwner1 $ call dodDao (Call @"Propose") params

  advanceToLevel (originLevel + 2*dodPeriod)

  forM_ addresses $ \n -> do

    runIO $ putTextLn $ show ("Voting for", n)
    getFreezeHistory dodDao n >>= \case
      Just fh -> runIO $ putTextLn $ show fh
      Nothing -> runIO $ putTextLn "No freeze history"

    let voteParam = NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = makeProposalKey params
          , vFrom = n
          }

    nowLevel <- getLevel
    runIO $ putTextLn $ ("Level= " <> (show nowLevel))

    let periodNum = div (nowLevel - originLevel) dodPeriod
    runIO $ putTextLn $ show ("In level", originLevel, nowLevel, periodNum)

    when (mod periodNum 2 == 1) $ do
      let waitTill = originLevel + (periodNum + 1)*dodPeriod
      runIO $ putTextLn $ show ("In proposal period, waiting till", waitTill)
      advanceToLevel waitTill

    withSender n $ call dodDao (Call @"Vote") [voteParam]

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 1)
  #-}
