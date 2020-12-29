-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.TreasuryDAO
  ( test_TreasuryDAO
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Time (sec)

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.TreasuryDAO
import Lorentz.Contracts.TreasuryDAO.Types
import Test.Common

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

      -- TODO: flush a config proposal
      -- TODO: check all fail scenario of proposal_check
      ]
  ]

validProposal :: (Monad m) => NettestImpl m -> m ()
validProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, _) <-
      originateBaseDaoWithBalance def config
          (\owner1_ owner2_ ->
              [ ((owner1_, DAO.unfrozenTokenId), 200)
              , ((owner2_, DAO.unfrozenTokenId), 100)
              ]
          )

  let proposal = tokenTransferProposalMetadata (toAddress dao) owner2 owner1 -- transfer token from owner2 -> owner1
      params t = DAO.ProposeParams
        { ppFrozenToken = t
        , ppProposalMetadata = proposal
        }
      expectedToken = fromInteger $ toInteger $ length $ lPackValueRaw proposal

  callFrom (AddressResolved owner1) dao (Call @"Propose") (params $ expectedToken - 1)
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

  callFrom (AddressResolved owner1) dao (Call @"Propose") (params $ expectedToken + 1)
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

  -- Expected token is 58 in this case
  _ <- createSampleProposal expectedToken proposal owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 117
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 83

flushTokenTransfer :: (Monad m) => NettestImpl m -> m ()
flushTokenTransfer = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <-
      originateBaseDaoWithBalance def config
          (\owner1_ owner2_ ->
              [ ((owner1_, DAO.unfrozenTokenId), 200)
              , ((owner2_, DAO.unfrozenTokenId), 100)
              ]
          )

  -- Set RegistryDAO as operator of the address that is mean to transfer the tokens
  let opParams = FA2.OperatorParam
        { opOwner = owner2
        , opOperator = toAddress dao
        , opTokenId = DAO.unfrozenTokenId
        }
  callFrom (AddressResolved owner2) dao (Call @"Update_operators") [FA2.AddOperator opParams]

  let proposal = tokenTransferProposalMetadata (toAddress dao) owner2 owner1 -- transfer token from owner2 -> owner1
      expectedToken = getTokensAmount proposal

  key1 <- createSampleProposal expectedToken proposal owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 117
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 83

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

-- TODO: [#86] Add custom entrypoints
flushXtzTransfer :: (Monad m) => NettestImpl m -> m ()
flushXtzTransfer = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig def config

  sendXtz (toAddress dao)


  let proposal = xtzTransferProposalMetadata owner2 -- transfer from dao to owner2
      expectedToken = getTokensAmount proposal

  key1 <- createSampleProposal expectedToken proposal owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 43
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 57

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

tokenTransferProposalMetadata :: Address -> Address -> Address -> TreasuryDaoProposalMetadata
tokenTransferProposalMetadata contractAddr fromAddr toAddr = NormalProposalType $ NormalProposal
  { npAgoraPostId = AgoraPostId 1
  , npTransfers =
      [ TokenTransferType $ TokenTransfer
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
      ]
  }

xtzTransferProposalMetadata :: Address -> TreasuryDaoProposalMetadata
xtzTransferProposalMetadata toAddr = TreasuryDaoProposalMetadata
  { npAgoraPostId = AgoraPostId 1
  , npTransfers =
      [ XtzTransferType $ XtzTransfer
          { xtAmount = toMutez 2
          , xtRecipient = toAddr
          }
      ]
  }

getTokensAmount :: TreasuryDaoProposalMetadata -> Natural
getTokensAmount pm = fromInteger $ toInteger $ length $ lPackValueRaw pm

createSampleProposal
  :: MonadNettest caps base m
  => Natural
  -> TreasuryDaoProposalMetadata
  -> Address
  -> TAddress (DAO.Parameter TreasuryDaoProposalMetadata TreasuryDaoExtraInterface)
  -> m (DAO.ProposalKey (TreasuryDaoProposalMetadata))
createSampleProposal t pm owner1 dao = do
  let params = DAO.ProposeParams
        { ppFrozenToken = t
        , ppProposalMetadata = pm
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)
