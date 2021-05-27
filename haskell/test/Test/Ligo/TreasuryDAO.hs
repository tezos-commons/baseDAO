-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.TreasuryDAO
  ( test_TreasuryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Michelson.Untyped.Entrypoints
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import Ligo.Util
import Test.Ligo.BaseDAO.Common
  ( DaoOriginateData(..), OriginateFn, checkTokenBalance, makeProposalKey
  , originateLigoDaoWithBalance, sendXtz )

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data TransferProposal = TransferProposal
  { tpAgoraPostId :: Natural
  , tpTransfers :: [TransferType]
  }

instance HasAnnotation TransferProposal where
  annOptions = baseDaoAnnOptions

customGeneric "TransferProposal" ligoLayout
deriving anyclass instance IsoValue TransferProposal

-- | Helper type for unpack/pack
type TreasuryDaoProposalMetadata = TransferProposal

-- | Testing a Treasury-like DAO. Ex. DNS Treasury
test_TreasuryDAO :: TestTree
test_TreasuryDAO = testGroup "TreasuryDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenarioOnEmulator "can propose a valid proposal" $
          \_emulated -> validProposal
      , nettestScenarioOnEmulator "can flush a Token transfer proposal" $
          \_emulated -> flushTokenTransfer
      , nettestScenarioOnEmulator "can flush a Xtz transfer proposal" $
          \_emulated -> flushXtzTransfer
      ]

  , testGroup "proposal_check:"
      [ nettestScenarioOnEmulator "fail when xtz transfer contains 0 mutez" $
          \_emulated -> proposalCheckFailZeroMutez

      -- TODO #260: check all fail scenario of proposal_check
      ]
  ]


metadataSize :: ByteString -> Natural
metadataSize md = fromIntegral $ BS.length md

validProposal :: HasCallStack => NettestScenario m
validProposal = uncapsNettest $ withFrozenCallStack do
  DaoOriginateData{..} <-
    originateTreasuryDaoWithBalance id $ \owner1_ owner2_ ->
      [ ((owner1_, frozenTokenId), 200)
      , ((owner2_, frozenTokenId), 200)
      ]
  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta -- 115

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams (proposalSize + 1) proposalMeta)
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dodDao

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams proposalSize proposalMeta)

  checkTokenBalance frozenTokenId dodDao dodOwner1 (200 + proposalSize)

flushTokenTransfer :: HasCallStack => NettestScenario m
flushTokenTransfer = uncapsNettest $ withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateTreasuryDaoWithBalance id $ \_ owner2_ ->
      [ ((owner2_, frozenTokenId), 100)
      ]

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodTokenContract) dodOwner2 dodOwner1 ]
        }
    proposalSize = metadataSize proposalMeta
    proposeParams = ProposeParams proposalSize proposalMeta

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! proposalSize)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting periods to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $ call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams dodOwner1

  checkTokenBalance frozenTokenId dodDao dodOwner1 proposalSize

  let
    upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  checkTokenBalance frozenTokenId dodDao dodOwner1 proposalSize
  checkTokenBalance frozenTokenId dodDao dodOwner2 120

flushXtzTransfer :: HasCallStack => NettestScenario m
flushXtzTransfer = uncapsNettest $ withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateTreasuryDaoWithBalance id $ \_ _ ->
      []

  sendXtz (toAddress dodDao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

  let
    proposalMeta amt = lPackValueRaw @TreasuryDaoProposalMetadata $
      TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType amt dodOwner2 ]
        }
    proposeParams amt = ProposeParams (metadataSize $ proposalMeta amt) $ proposalMeta amt

  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! (metadataSize $ proposalMeta 3))

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $ do
  -- due to smaller than min_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 1)
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dodDao

  -- due to bigger than max_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 6)
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dodDao

    call dodDao (Call @"Propose") (proposeParams 3)
  let key1 = makeProposalKey (proposeParams 3) dodOwner1

  checkTokenBalance (frozenTokenId) dodDao dodOwner1 43

  let
    upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: check xtz balance


proposalCheckFailZeroMutez :: HasCallStack => NettestScenario m
proposalCheckFailZeroMutez = uncapsNettest $ withFrozenCallStack do
  DaoOriginateData{..} <-
    originateTreasuryDaoWithBalance
      (\store -> setExtra @Natural [mt|min_xtz_amount|] 0 store) $
      \owner1_ owner2_ ->
        [ ((owner1_, frozenTokenId), 200)
        , ((owner2_, frozenTokenId), 200)
        ]
  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType 0 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams proposalSize proposalMeta)
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dodDao

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

originateTreasuryDaoWithBalance
 :: forall caps base m. (MonadNettest caps base m)
 => (FullStorage -> FullStorage)
 -> (Address -> Address -> [(LedgerKey, LedgerValue)]) -> OriginateFn m
originateTreasuryDaoWithBalance modifyStorageFn bal =
  let fs = fromVal ($(fetchValue @FullStorage "haskell/test/treasuryDAO_storage.tz" "TREASURY_STORAGE_PATH"))
      FullStorage'{..} = fs
        & setExtra @Natural [mt|frozen_scale_value|] 1
        & setExtra @Natural [mt|frozen_extra_value|] 0
        & setExtra @Natural [mt|slash_scale_value|] 1
        & setExtra @Natural [mt|slash_division_value|] 1
        & setExtra @Natural [mt|max_proposal_size|] 1000
        & setExtra @Natural [mt|min_xtz_amount|] 2
        & setExtra @Natural [mt|max_xtz_amount|] 5
        & modifyStorageFn

  in originateLigoDaoWithBalance (sExtra fsStorage)
      (fsConfig
        { cMinQuorumThreshold = fromIntegral $ mkQuorumThreshold 1 100
        , cPeriod = 10
        , cProposalFlushTime = 20
        , cProposalExpiredTime = 30
        }
      ) bal
