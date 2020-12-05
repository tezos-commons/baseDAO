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
  ]

validProposal :: (Monad m) => NettestImpl m -> m ()
validProposal = uncapsNettest $ do
  (consumer :: TAddress MText) <- originateSimple "consumer" [] contractConsumer
  dao :/ (owner1, ()) <- originateBaseDaoWithConfig def config

  -- Fail due to proposing new content require 50 token.
  callFrom (AddressResolved owner1) dao (Call @"Propose") (DAO.ProposeParams
    { ppFrozenToken = 20
    , ppProposalMetadata = sampleMetadataContent $ toAddress consumer
    }) & expectCustomError_ #fAIL_PROPOSAL_CHECK

  callFrom (AddressResolved owner1) dao (Call @"Propose") (DAO.ProposeParams
    { ppFrozenToken = 15
    , ppProposalMetadata = sampleMetadataBalance $ toAddress consumer
    })

  checkTokenBalance (DAO.frozenTokenId) dao owner1 15
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 85

flushAcceptedProposals :: (Monad m) => NettestImpl m -> m ()
flushAcceptedProposals = uncapsNettest $ do
  (consumer :: TAddress MText) <- originateSimple "consumer" [] contractConsumer
  dao :/ (owner1, owner2) :/ TestSetup{ tsAdmin = admin }
    <- originateBaseDaoWithConfig def config

  callFrom (AddressResolved admin) dao (Call @"Set_voting_period") 20
  callFrom (AddressResolved admin) dao (Call @"Set_quorum_threshold") 1

  -- | Accepted Proposals
  key1 <- createSampleProposal (sampleMetadataBalance $ toAddress consumer) owner1 dao
  checkTokenBalance (DAO.frozenTokenId) dao owner1 10

  let upvote = DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }
      downvote = DAO.VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        }
  callFrom (AddressResolved owner2) dao (Call @"Vote") [upvote, downvote]
  checkTokenBalance (DAO.frozenTokenId) dao owner2 3
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 97

  advanceTime (sec 21)
  callFrom (AddressResolved admin) dao (Call @"Flush") ()

  checkTokenBalance (DAO.frozenTokenId) dao owner1 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 100 -- proposer

  checkTokenBalance (DAO.frozenTokenId) dao owner2 0
  checkTokenBalance (DAO.unfrozenTokenId) dao owner2 100 -- voter

  -- Consumer contract is used in decision lambda and should contain accepted proposal
  -- description.
  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [([mt|Balance Item|] :: MText)])

  -- TODO [#31]: add a check on proposals counter

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
  => GameDaoProposalMetadata -> Address -> TAddress Parameter -> m ByteString
createSampleProposal pm owner1 dao = do
  let params = DAO.ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = pm
        }

  callFrom (AddressResolved owner1) dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)
