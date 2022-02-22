-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.TreasuryDAO
  ( test_TreasuryDAO
  ) where

import Universum

import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Tezos.Address
import Morley.Util.Named
import Test.Cleveland
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Contract (baseDAOStorageLigo)
import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.TreasuryDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.TreasuryDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Testing a Treasury-like DAO. Ex. DNS Treasury
test_TreasuryDAO :: TestTree
test_TreasuryDAO = testGroup "TreasuryDAO Tests"
  [ testGroup "Proposal creator:"
      [ testScenario "can propose a valid proposal" $ scenario $
          validProposal
      , testScenario "can flush a Token transfer proposal" $ scenario $
          flushTokenTransfer
      , testScenario "can flush a Xtz transfer proposal" $ scenario $
          flushXtzTransfer
      , testScenario "can flush a Update_guardian proposal" $ scenario $
          flushUpdateGuardian
      , testScenario "can flush a Update_contract_delegate proposal" $ scenario $
          flushUpdateContractDelegate
      ]

  , testGroup "proposal_check:"
      [ testScenario "fail when xtz transfer contains 0 mutez" $ scenario $
          proposalCheckFailZeroMutez
      , testScenario "fail when proposal size is bigger than max" $ scenario $
          proposalCheckBiggerThanMaxProposalSize
      ]
  ]

validProposal
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
validProposal = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta -- 115

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod + 1)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 (proposalSize + 1) proposalMeta)
    & expectFailedWith (failProposalCheck, incorrectTokenAmountErrMsg)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 proposalSize proposalMeta)

  checkBalance dodDao dodOwner1 (proposalSize)

flushTokenTransfer
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
flushTokenTransfer = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodTokenContract) dodOwner2 dodOwner1 ]
        }
    proposalSize = metadataSize proposalMeta
    proposeParams = ProposeParams dodOwner1 proposalSize proposalMeta

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! proposalSize)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 20)

  -- Advance one voting periods to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $ call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams

  checkBalance dodDao dodOwner1 proposalSize

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  checkBalance dodDao dodOwner1 proposalSize
  checkBalance dodDao dodOwner2 20

flushXtzTransfer
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
flushXtzTransfer = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold
  originationLevel <- getOriginationLevel dodDao

  let
    proposalMeta amt = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType amt dodOwner2 ]
        }
    proposeParams amt = ProposeParams dodOwner1 (metadataSize $ proposalMeta amt) $ proposalMeta amt

  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! (metadataSize $ proposalMeta 3))

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 10)
  -- Advance one voting period to a proposing stage.
  sendXtz (TAddress $ toAddress dodDao)
  advanceToLevel (originationLevel + dodPeriod)

  withSender dodOwner1 $ do
    -- due to smaller than min_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 1)
      & expectFailedWith (failProposalCheck, tooSmallXtzErrMsg)

    -- due to bigger than max_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 6)
      & expectFailedWith (failProposalCheck, tooLargeXtzErrMsg)

    call dodDao (Call @"Propose") (proposeParams 3)
  let key1 = makeProposalKey (proposeParams 3)

  checkBalance dodDao dodOwner1 47

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (originationLevel + 2*dodPeriod + 1)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  --TODO: check xtz balance

flushUpdateGuardian
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
flushUpdateGuardian = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Update_guardian dodOwner2
    proposeParams = ProposeParams dodOwner1 (metadataSize $ proposalMeta) $ proposalMeta

  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! (metadataSize $ proposalMeta))

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 10)
  sendXtz (TAddress $ toAddress dodDao)
  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  checkGuardian dodDao dodOwner2

flushUpdateContractDelegate
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
flushUpdateContractDelegate = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold
  registerDelegate dodOperator2
  case dodOperator2 of
    KeyAddress delegate -> do
      let
        proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
          Update_contract_delegate $ Just delegate
        proposeParams = ProposeParams dodOwner1 (metadataSize $ proposalMeta) $ proposalMeta

      -- Freeze in initial voting stage.
      withSender dodOwner1 $
        call dodDao (Call @"Freeze") (#amount :! (metadataSize $ proposalMeta))

      withSender dodOwner2 $
        call dodDao (Call @"Freeze") (#amount :! 10)
      sendXtz (TAddress $ toAddress dodDao)
      -- Advance one voting period to a proposing stage.
      startLevel <- getOriginationLevel dodDao
      advanceToLevel (startLevel + dodPeriod)

      withSender dodOwner1 $
        call dodDao (Call @"Propose") proposeParams
      let key1 = makeProposalKey proposeParams

      let
        upvote = NoPermit VoteParam
            { vFrom = dodOwner2
            , vVoteType = True
            , vVoteAmount = 1
            , vProposalKey = key1
            }

      -- Advance one voting period to a voting stage.
      advanceToLevel (startLevel + 2*dodPeriod)
      withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
      -- Advance one voting period to a proposing stage.
      proposalStart <- getProposalStartLevel dodDao key1
      advanceToLevel (proposalStart + 2*dodPeriod + 1)
      withSender dodAdmin $ call dodDao (Call @"Flush") 100
      getDelegate dodDao @@== (Just delegate)
    _ -> error "impossible"

proposalCheckFailZeroMutez
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
proposalCheckFailZeroMutez = withFrozenCallStack do
  DaoOriginateData{..} <-
    originateTreasuryDao
      (\store -> setExtra @Natural [mt|min_xtz_amount|] 0 store)
      defaultQuorumThreshold

  startLevel <- getOriginationLevel dodDao

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType 0 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 proposalSize proposalMeta)
      & expectFailedWith (failProposalCheck, zeroMutezErrMsg)

proposalCheckBiggerThanMaxProposalSize
  :: forall caps base m. (MonadCleveland caps base m, HasCallStack)
  => m ()
proposalCheckBiggerThanMaxProposalSize = withFrozenCallStack do
  DaoOriginateData{..} <-
    originateTreasuryDao id defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let
    largeProposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal 1 $
        [tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 | (_ :: Integer) <- [1..10]]
    largeProposalSize = metadataSize largeProposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! largeProposalSize)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 largeProposalSize largeProposalMeta)
      & expectFailedWith (failProposalCheck, tooLargeProposalErrMsg)


--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

xtzTransferType :: Word32 -> Address -> TransferType
xtzTransferType amt toAddr = Xtz_transfer_type XtzTransfer
  { xtAmount = toMutez amt
  , xtRecipient = toAddr
  }

tokenTransferType :: Address -> Address -> Address -> TransferType
tokenTransferType contractAddr fromAddr toAddr = Token_transfer_type TokenTransfer
  { ttContractAddress = contractAddr
  , ttTransferList =
      [ FA2.TransferItem
      { tiFrom = fromAddr
      , tiTxs = [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = FA2.theTokenId
          , tdAmount = 10
          } ]
      } ]
  }

originateTreasuryDao
 :: forall caps base m. (MonadCleveland caps base m)
 => (TreasuryFullStorage -> TreasuryFullStorage)
 -> OriginateFn 'Treasury m
originateTreasuryDao modifyStorageFn =
  let fs = baseDAOStorageLigo
      FullStorageP {..} = fs
        -- & setExtra @Natural [mt|frozen_scale_value|] 1
        -- & setExtra @Natural [mt|frozen_extra_value|] 0
        -- & setExtra @Natural [mt|slash_scale_value|] 1
        -- & setExtra @Natural [mt|slash_division_value|] 1
        -- & setExtra @Natural [mt|max_proposal_size|] 1000
        -- & setExtra @Natural [mt|min_xtz_amount|] 2
        -- & setExtra @Natural [mt|max_xtz_amount|] 5
        -- & modifyStorageFn

  in originateLigoDaoWithConfig @'Treasury (undefined :: TreasuryExtra)
      (fsConfig
        { cMinQuorumThreshold = fromIntegral $ mkQuorumThreshold 1 100
        , cPeriod = 10
        , cProposalFlushLevel = 20
        , cProposalExpiredLevel = 30
        }
      )
