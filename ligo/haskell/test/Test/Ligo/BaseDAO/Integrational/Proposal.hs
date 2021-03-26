-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ligo.BaseDAO.Integrational.Proposal
  ( test_IntegrationalBaseDAOProposal
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz hiding (now)
import Lorentz.Test
import Test.Tasty (TestTree, testGroup)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Test.Tasty.HUnit (testCase)
import Named ((!))

import Test.Ligo.BaseDAO.Integrational.Common
import Ligo.BaseDAO.Types
import Michelson.Untyped (unsafeBuildEpName)
import Michelson.Test.Integrational
import Util.Named

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_IntegrationalBaseDAOProposal :: TestTree
test_IntegrationalBaseDAOProposal = testGroup "BaseDAO Integrational Tests"
  [ testGroup "Integrational Tests for checking storage"
      [ testCase "contract correctly tracks freeze-unfreeze history" $
          integrationalTestExpectation $ checkFreezeHistoryTracking
      ]
  ]

checkFreezeHistoryTracking :: IntegrationalScenarioM ()
checkFreezeHistoryTracking  = do
  let frozen_scale_value = 2
  let frozen_extra_value = 0

  now <- use isNow

  withOriginated 2 (\(admin:_) ->
    mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold 10
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #customEps mempty
    ) $ \(admin:wallet1:_) baseDao -> let
      proposalMeta1 = DynamicRec $ Map.fromList $ [([mt|key1|], "val"), ([mt|key2|], "val")] -- 44
      proposalSize1 = metadataSize proposalMeta1

      in do
        let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

        tTransfer  (#from .! admin) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "mint") (toVal (MintParam wallet1 unfrozenTokenId 1000)) -- Mint some tokens for wallet1.
        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "freeze") (toVal requiredFrozen)

        rewindTime 21

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta1))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let fh = Map.lookup wallet1 (unBigMap $ sFreezeHistory $ fsStorage storage)
            let expected = AddressFreezeHistory
                  { fhCurrentPeriodNum = 2
                  , fhCurrentUnstaked = 0
                  , fhStaked = requiredFrozen
                  , fhPastUnstaked = 0
                  }
            when (fh /= (Just expected)) $ Left $ CustomTestError "BaseDAO contract did not stake tokens as expected"

        rewindTime 20

        tTransfer  (#from .! admin) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "flush") (toVal (1 :: Natural))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            -- We expect all the staked tokens to be unstaked after propoal rejection/accept.
            let fh = Map.lookup wallet1 (unBigMap $ sFreezeHistory $ fsStorage storage)
            let expected = AddressFreezeHistory
                  { fhCurrentPeriodNum = 4
                  , fhCurrentUnstaked = 0
                  , fhStaked = 0
                  , fhPastUnstaked = requiredFrozen
                  }
            when (fh /= (Just expected)) $
              Left $ CustomTestError "BaseDAO contract did not unstake tokens after voting period"
    where
      metadataSize :: ProposalMetadata -> Natural
      metadataSize md = fromIntegral $ BS.length $ lPackValueRaw md
