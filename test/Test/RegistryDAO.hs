-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.RegistryDAO
  ( test_RegistryDAO
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Time (sec)

import BaseDAO.ShareTest.Common (sendXtz)
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Lorentz.Contracts.RegistryDAO
import Lorentz.Contracts.RegistryDAO.Types
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Testing a registry-like DAO. Ex. DNS registry
test_RegistryDAO :: TestTree
test_RegistryDAO = testGroup "RegistryDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenario "can propose a valid proposal" validProposal

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can propose a valid configuration proposal" $
          \_emulated -> validConfigProposal
      ]
  ]

configBS :: DAO.Config
  (RegistryDaoContractExtra ByteString ByteString)
  (RegistryDaoProposalMetadata ByteString ByteString)
  Empty
configBS = config

type Parameter =
  DAO.Parameter (RegistryDaoProposalMetadata ByteString ByteString) Empty

validProposal :: (Monad m) => NettestImpl m -> m ()
validProposal = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig def configBS

  let params t = DAO.ProposeParams
        { ppFrozenToken = t
        , ppProposalMetadata = longNormalProposalMetadata
        }
      expectedToken = fromInteger $ toInteger $ length $ lPackValueRaw longNormalProposalMetadata

  withSender (AddressResolved owner1) $ do
    call dao (Call @"Propose") (params $ expectedToken - 1)
      & expectCustomError_ #fAIL_PROPOSAL_CHECK

    call dao (Call @"Propose") (params $ expectedToken + 1)
      & expectCustomError_ #fAIL_PROPOSAL_CHECK

  -- Expected token is 58 in this case
  _ <- createSampleProposal (getTokensAmount longNormalProposalMetadata) longNormalProposalMetadata owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 62
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 38

validConfigProposal :: (Monad m) => NettestImpl m -> m ()
validConfigProposal = uncapsNettest $ do
  ((owner1, _), (owner2, _), dao, admin) <- originateBaseDaoWithConfig def configBS

  sendXtz owner1 DefEpName () -- fixes "Balance is too low" error in CI.

  let
    (configMetadata :: RegistryDaoProposalMetadata ByteString ByteString) = ConfigProposalType $ ConfigProposal
        { cpFrozenScaleValue = Just 1 -- a
        , cpFrozenExtraValue = Just 5 -- b
        , cpSlashScaleValue = Just 1 -- c
        , cpSlashDivisionValue = Just 2 -- d
        , cpMaxProposalSize = Just 62 -- s_max
        }

  key1 <- createSampleProposal (getTokensAmount configMetadata) configMetadata owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 33
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 67

  let
    upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote]

  advanceTime (sec 20)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- Fail due too big proposal size
  _ <- createSampleProposal ((getTokensAmount longNormalProposalMetadata) + 5) longNormalProposalMetadata owner1 dao
    & expectCustomError_ #fAIL_PROPOSAL_CHECK

  -- Expected token is 58 + 5 in this case
  _ <- createSampleProposal ((getTokensAmount normalProposalMetadata) + 5) normalProposalMetadata owner1 dao

  checkTokenBalance (DAO.frozenTokenId) dao owner1 63
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 37

  advanceTime (sec 20)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  -- Only half are returned
  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 69

--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

-- metadata size = 58
normalProposalMetadata :: RegistryDaoProposalMetadata ByteString ByteString
normalProposalMetadata = NormalProposalType $ NormalProposal
  { npAgoraPostId = AgoraPostId 1
  , npDiff =
      [ RegistryUpdateItem
        { ruKey = lPackValueRaw [mt|somedomain.xyz|]
        , ruNewValue = Just (lPackValueRaw [mt|durden|])
        }
      ]
  }

-- metadata size = 62
longNormalProposalMetadata :: RegistryDaoProposalMetadata ByteString ByteString
longNormalProposalMetadata = NormalProposalType $ NormalProposal
  { npAgoraPostId = AgoraPostId 2
  , npDiff =
      [ RegistryUpdateItem
        { ruKey = lPackValueRaw [mt|somelongdomain.xyz|]
        , ruNewValue = Just (lPackValueRaw [mt|durden|])
        }
      ]
  }

getTokensAmount :: RegistryDaoProposalMetadata ByteString ByteString -> Natural
getTokensAmount pm = fromInteger $ toInteger $ length $ lPackValueRaw pm

createSampleProposal
  :: MonadNettest caps base m
  => Natural
  -> RegistryDaoProposalMetadata ByteString ByteString
  -> Address
  -> TAddress Parameter
  -> m (DAO.ProposalKey (RegistryDaoProposalMetadata ByteString ByteString))
createSampleProposal t pm owner1 dao = do
  let params = DAO.ProposeParams
        { ppFrozenToken = t
        , ppProposalMetadata = pm
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)
