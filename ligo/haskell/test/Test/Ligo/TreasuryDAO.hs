-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
module Test.Ligo.TreasuryDAO
  ( test_TreasuryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Lorentz
import Michelson.Untyped.Entrypoints
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Time (sec)

import BaseDAO.ShareTest.Common (OriginateFn, checkTokenBalance, makeProposalKey, sendXtz)
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.TreasuryDAO.Types
import Ligo.BaseDAO.Types
import Ligo.Util
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Testing a Treasury-like DAO. Ex. DNS Treasury
test_TreasuryDAO :: TestTree
test_TreasuryDAO = testGroup "TreasuryDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenario "can propose a valid proposal" validProposal
      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can flush a 'Token transfer' proposal" $
          \_emulated -> flushTokenTransfer
      , nettestScenarioOnEmulator "can flush a 'Xtz transfer' proposal" $
          \_emulated -> flushXtzTransfer

      -- -- TODO: flush a config proposal
      -- -- TODO: check all fail scenario of proposal_check
      ]
  ]


metadataSize :: DynamicRec "pm" -> Natural
metadataSize md = fromIntegral $ BS.length $ lPackValueRaw md

validProposal :: (Monad m, HasCallStack) => NettestImpl m -> m ()
validProposal = uncapsNettest $ withFrozenCallStack do
  ((owner1, _), (owner2, _), dao, _) <-
    withDefaultStartup $ originateTreasuryDaoWithBalance $ \owner1_ owner2_ ->
      [ ((owner1_, DAO.unfrozenTokenId), 200)
      , ((owner2_, DAO.unfrozenTokenId), 200)
      ]
  let
    proposalMeta = DynamicRec $ Map.fromList $
      [ ([mt|agora_post_id|], lPackValueRaw @Natural 1)
      , ([mt|transfers|], lPackValueRaw @([TransferType])
          [ tokenTransferType (toAddress dao) owner1 owner2
          ]
        )
      ]
    proposalSize = metadataSize proposalMeta

  callFrom (AddressResolved owner1) dao (Call @"Propose") (ProposeParams (proposalSize + 1) proposalMeta)
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

  callFrom (AddressResolved owner1) dao (Call @"Propose") (ProposeParams proposalSize proposalMeta)

  checkTokenBalance (DAO.frozenTokenId) dao owner1 166
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 34

flushTokenTransfer :: (Monad m, HasCallStack) => NettestImpl m -> m ()
flushTokenTransfer = uncapsNettest $ withFrozenCallStack $ do
  ((owner1, _), (owner2, _), dao, admin) <-
    withDefaultStartup $ originateTreasuryDaoWithBalance $ \owner1_ owner2_ ->
      [ ((owner1_, DAO.unfrozenTokenId), 200)
      , ((owner2_, DAO.unfrozenTokenId), 100)
      ]

  -- Set RegistryDAO as operator of the address that is mean to transfer the tokens
  let opParams = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = toAddress dao
        , opTokenId = DAO.unfrozenTokenId
        }
  callFrom (AddressResolved owner2) dao (Call @"Update_operators") [FA2.AddOperator opParams]


  let
    proposalMeta = DynamicRec $ Map.fromList $
      [ ([mt|agora_post_id|], lPackValueRaw @Natural 1)
      , ([mt|transfers|], lPackValueRaw @([TransferType])
          [ tokenTransferType (toAddress dao) owner2 owner1
          ]
        )
      ]
    proposalSize = metadataSize proposalMeta
    proposeParams = ProposeParams proposalSize proposalMeta

  callFrom (AddressResolved owner1) dao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams owner1

  checkTokenBalance (DAO.frozenTokenId) dao owner1 166
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 34

  let
    upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]
  advanceTime (sec 20)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 210
  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 90

flushXtzTransfer :: (Monad m, HasCallStack) => NettestImpl m -> m ()
flushXtzTransfer = uncapsNettest $ withFrozenCallStack $ do
  ((owner1, _), (owner2, _), dao, admin) <-
    withDefaultStartup $ originateTreasuryDaoWithBalance $ \owner1_ owner2_ ->
      [ ((owner1_, DAO.unfrozenTokenId), 100)
      , ((owner2_, DAO.unfrozenTokenId), 100)
      ]

  sendXtz (toAddress dao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

  let
    proposalMeta amt = DynamicRec $ Map.fromList $
      [ ([mt|agora_post_id|], lPackValueRaw @Natural 1)
      , ([mt|transfers|], lPackValueRaw @([TransferType])
          [ xtzTransferType amt owner2 -- transfer from dao to owner2
          ]
        )
      ]
    proposeParams amt = ProposeParams (metadataSize $ proposalMeta amt) $ proposalMeta amt

  -- due to smaller than y (min mutez allow)
  callFrom (AddressResolved owner1) dao (Call @"Propose") (proposeParams 1)
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

  -- due to bigger than z (max mutez allow)
  callFrom (AddressResolved owner1) dao (Call @"Propose") (proposeParams 6)
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

  callFrom (AddressResolved owner1) dao (Call @"Propose") (proposeParams 3)
  let key1 = makeProposalKey (proposeParams 3) owner1

  checkTokenBalance (DAO.frozenTokenId) dao owner1 94
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 6

  let
    upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote]
  advanceTime (sec 20)
  callFrom (AddressResolved admin) dao (Call @"Flush") 100

--   -- TODO: check xtz balance

--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

xtzTransferType :: Word32 -> Address -> TransferType
xtzTransferType amt toAddr = Xtz_transfer_type $ XtzTransfer
  { xtAmount = toMutez amt
  , xtRecipient = toAddr
  }

tokenTransferType :: Address -> Address -> Address -> TransferType
tokenTransferType contractAddr fromAddr toAddr = Token_transfer_type $ TokenTransfer
  { ttContractAddress = contractAddr
  , ttTransferList =
      [ FA2.TransferItem
      { tiFrom = fromAddr
      , tiTxs = [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = DAO.unfrozenTokenId
          , tdAmount = 10
          } ]
      } ]
  }

originateTreasuryDaoWithBalance
 :: forall caps base m. (MonadNettest caps base m)
 => (Address -> Address -> [(LedgerKey, LedgerValue)]) -> OriginateFn ParameterL m
originateTreasuryDaoWithBalance bal =
  let fs = $(fetchValue @FullStorage "ligo/haskell/test/treasuryDAO_storage.tz" "TREASURY_STORAGE_PATH")
      configStore = fsConfigured fs
      testExtra = (sExtra $ csStorage configStore)
        & setExtra @Natural [mt|a|] 1
        & setExtra @Natural [mt|b|] 0
        & setExtra @Natural [mt|c|] 1
        & setExtra @Natural [mt|d|] 1
        & setExtra @Natural [mt|s_max|] 1000
        & setExtra @Natural [mt|y|] 2
        & setExtra @Natural [mt|z|] 5
      customEps = Map.toList . unBigMap . ssStoredEntrypoints $ fsStartup fs

  in originateLigoDaoWithBalance customEps testExtra (csConfig configStore) bal
  where
    setExtra :: forall a n. NicePackedValue a => MText -> a -> DynamicRec n -> DynamicRec n
    setExtra key v extra =
      DynamicRec $ Map.insert key (lPackValueRaw v) (unDynamic extra)
