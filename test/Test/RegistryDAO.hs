-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

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
import Data.Map as Map
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Lorentz.Contracts.RegistryDAO
import Lorentz.Contracts.RegistryDAO.Types
import Lorentz.Test.Consumer
import Test.Common
import Tezos.Core (Timestamp(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Testing a registry-like DAO. Ex. DNS registry
test_RegistryDAO :: TestTree
test_RegistryDAO = testGroup "RegistryDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenario "can propose a valid proposal" validProposal
      , nettestScenario "onchain view to get registry value works" getRegistryValue

      -- TODO [#47]: Disable running in real network due to time-sensitive operations
      , nettestScenarioOnEmulator "can propose a valid configuration proposal" $
          \_emulated -> validConfigProposal
      ]
  ]

configBS :: DAO.Config
  (RegistryDaoContractExtra ByteString ByteString)
  (RegistryDaoProposalMetadata ByteString ByteString)
  (RegistryDAOCustomParam ByteString ByteString)
configBS = config

type Parameter =
  DAO.Parameter (RegistryDaoProposalMetadata ByteString ByteString) (RegistryDAOCustomParam ByteString ByteString)

getRegistryValue :: (Monad m) => NettestImpl m -> m ()
getRegistryValue = uncapsNettest $ do
  -- To check the view, we put a test value in registry during contract initialization
  -- so that we don't have to create and execute a proposal to put the value there. Some
  -- dummy values of proposal key and timestamp is used to create the corresponding registry
  -- entry.
  let dummyHash = HashUnsafe ""
  let dummyKey = encodeUtf8 @Text "dummyKey"
  let dummyVal = encodeUtf8 @Text "dummyVal"
  let registryEntry = RegistryEntry (Just dummyVal) dummyHash (Timestamp 0)
  let initExtra = def { ceRegistry = BigMap $ Map.fromList [(dummyKey, registryEntry)] }
  (_, _, dao, _) <- originateBaseDaoWithConfig initExtra configBS

  consumer <- originateSimple "consumer" [] (contractConsumer @(ByteString, (Maybe ByteString)))
  call dao (Call @"CallCustom") (LookupRegistry $ mkView dummyKey consumer)
  checkStorage (AddressResolved $ unTAddress consumer) (toVal [(dummyKey, Just dummyVal)])

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
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK

    call dao (Call @"Propose") (params $ expectedToken + 1)
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK

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
        { cpFrozenScaleValue = Just 1
        , cpFrozenExtraValue = Just 5
        , cpSlashScaleValue = Just 1
        , cpSlashDivisionValue = Just 2
        , cpMaxProposalSize = Just 62
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
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK

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
