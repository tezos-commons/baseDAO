-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Integrational.RegistryDAO
  ( test_IntegrationalRegistryDAO
  ) where

import Universum hiding (compare, drop, (>>))

import qualified Data.Map as Map
import Lorentz
import Lorentz.Test
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import Lorentz.Contracts.RegistryDAO
import Lorentz.Contracts.RegistryDAO.Types
import Test.Common
import Test.Integrational.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_IntegrationalRegistryDAO :: TestTree
test_IntegrationalRegistryDAO = testGroup "RegistryDAO Integrational Tests"
  [ testGroup "Integrational Tests for checking storage"
      [ testCase "Flush an accepted proposal should update the registry" $
          integrationalTestExpectation $ flushProposalIntegrational
      ]
  ]

flushProposalIntegrational :: IntegrationalScenarioM ()
flushProposalIntegrational = do
  ((owner1, _), (owner2, _), dao, admin) <- lOriginateBaseDao def (config @ByteString @ByteString)
  withSender admin $ lCallEP dao (Call @"Set_voting_period") 20
  withSender admin $ lCallEP dao (Call @"Set_quorum_threshold") 1

  let
    expectedK = lPackValueRaw [mt|somelongdomain.xyz|]
    expectedV = Just (lPackValueRaw [mt|durden|])

    proposal :: RegistryDaoProposalMetadata ByteString ByteString
    proposal = NormalProposalType $ NormalProposal
      { npAgoraPostId = AgoraPostId 2
      , npDiff =
          [ RegistryUpdateItem
            { ruKey = expectedK
            , ruNewValue = expectedV
            }
          ]
      }

    params t = DAO.ProposeParams
        { ppFrozenToken = t
        , ppProposalMetadata = proposal
        }

    expectedToken = fromInteger $ toInteger $ length $ lPackValueRaw proposal

  setNow $ timestampFromSeconds 1000
  withSender owner1 $ lCallEP dao (Call @"Propose") (params expectedToken)

  let key1 = makeProposalKey (params expectedToken) owner1
      upvote = DAO.NoPermit DAO.VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  withSender owner2 $ lCallEP dao (Call @"Vote") [upvote]
  rewindTime 20

  withSender owner1 $ lCallEP dao (Call @"Flush") Nothing
  let updatedTime = timestampFromSeconds 1000
  setNow updatedTime

  let expected = BigMap $ Map.fromList $ [
          ( expectedK, RegistryEntry
              { reValue = expectedV
              , reAffectedProposalKey = key1
              , reLastUpdated = timestampFromSeconds 1020
              }
          )
        ]

  lExpectStorage @(DAO.Storage
      (RegistryDaoContractExtra ByteString ByteString)
      (RegistryDaoProposalMetadata ByteString ByteString)
    ) dao $ \storage ->
        when ((ceRegistry $ DAO.sExtra storage) /= expected) $
                Left $ CustomTestError "RegistryDAO's registry is not updated after a proposal is accepted."
