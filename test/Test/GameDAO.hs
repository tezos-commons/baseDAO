-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.GameDAO
  ( test_GameDAO
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Time (sec)

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Lorentz.Contracts.GameDAO
import Test.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_GameDAO :: TestTree
test_GameDAO = testGroup "GameDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenario "can propose a valid proposal" validProposal
      ]
  , testGroup "Admin"
      [ nettestScenarioOnEmulator "can flush proposals that got accepted" $
          \_emulated -> flushAcceptedProposals
      ]
  , testGroup "CustomCall"
      [ nettestScenario "can call default entrypoint" callDefaultEp
      ]
  ]

validProposal :: (Monad m) => NettestImpl m -> m ()
validProposal = uncapsNettest $ do
  (consumer :: TAddress MText) <- originateSimple "consumer" [] contractConsumer
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig def config

  -- Fail due to proposing new content require 50 token.
  withSender (AddressResolved owner1) $ do
    call dao (Call @"Propose")
      (DAO.ProposeParams
        { ppFrozenToken = 20
        , ppProposalMetadata = sampleMetadataContent $ toAddress consumer
        }) & expectCustomError_ #fAIL_PROPOSAL_CHECK

    call dao (Call @"Propose")
      (DAO.ProposeParams
       { ppFrozenToken = 15
       , ppProposalMetadata = sampleMetadataBalance $ toAddress consumer
       })

  checkTokenBalance (DAO.frozenTokenId) dao owner1 15
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 85

flushAcceptedProposals :: (Monad m) => NettestImpl m -> m ()
flushAcceptedProposals = uncapsNettest $ do
  (consumer :: TAddress MText) <- originateSimple "consumer" [] contractConsumer
  ((owner1, _), (owner2, _), dao, admin)
    <- originateBaseDaoWithConfig def config

  -- | Accepted Proposals
  key1 <- createSampleProposal (sampleMetadataBalance $ toAddress consumer) owner1 dao
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10

  let upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }
      downvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote, downvote]
  checkTokenBalance (DAO.frozenTokenId) dao owner2 3
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 97

  advanceTime (sec 21)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 100 -- proposer

  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

  -- Consumer contract is used in decision lambda and should contain accepted proposal
  -- description.
  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [([mt|Balance Item|] :: MText)])

  -- TODO [#31]: add a check on proposals counter

callDefaultEp :: (Monad m) => NettestImpl m -> m ()
callDefaultEp = uncapsNettest $ do
  ((owner1, _), _, dao, _) <- originateBaseDaoWithConfig def config

  withSender (AddressResolved owner1) $ call dao CallDefault ()

-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------
sampleMetadataBalance :: Address -> GameDaoProposalMetadata
sampleMetadataBalance consumerAddr = GameDaoProposalMetadata
  { pmProposalType = BalanceType BalanceChange
      { bcItemChanges = [ ItemChange
          { icItemName = [mt|Ganto|]
          , icChangelogs =
              [ [mt|Ganto now has 10 charges.|]
              , [mt|Ganto can now target enemey to reduce health regen.|]
              ]
          }]
      , bcHeroChanges = []
      }
  , pmProposalDescription = [mt|Balance Item|]
  , pmConsumerAddr = consumerAddr
  }

sampleMetadataContent :: Address -> GameDaoProposalMetadata
sampleMetadataContent consumerAddr = GameDaoProposalMetadata
  { pmProposalType = NewType [ NewContent
      { ncContentName = [mt|TireDide|]
      , ncContentDescription =
          [ [mt|New Halloween event called TireDide.|]
          , [mt|Each players play in a team of 5 fighting each other to gain the most candies.|]
          ]
      } ]
  , pmProposalDescription = [mt|New Content Update|]
  , pmConsumerAddr = consumerAddr
  }

createSampleProposal
  :: MonadNettest caps base m
  => GameDaoProposalMetadata
  -> Address
  -> TAddress Parameter
  -> m (DAO.ProposalKey GameDaoProposalMetadata)
createSampleProposal pm owner1 dao = do
  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = pm
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)
