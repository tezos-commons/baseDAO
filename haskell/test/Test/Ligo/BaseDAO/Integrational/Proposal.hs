-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ligo.BaseDAO.Integrational.Proposal
  ( test_IntegrationalBaseDAOProposal
  ) where

import Universum hiding (compare, drop, (>>))

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Lorentz hiding (now)
import Lorentz.Test
import Named ((!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Ligo.BaseDAO.Types
import Michelson.Test.Integrational
import Michelson.Untyped (unsafeBuildEpName)
import Test.Ligo.BaseDAO.Common (dummyFA2Contract, makeProposalKey)
import Test.Ligo.BaseDAO.Integrational.Common
import Util.Named

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_IntegrationalBaseDAOProposal :: TestTree
test_IntegrationalBaseDAOProposal = testGroup "BaseDAO Integrational Tests"
  [ testGroup "Integrational Tests for checking storage"
      [ testCase "contract correctly tracks freeze-unfreeze history" $
          integrationalTestExpectation $ checkFreezeHistoryTracking
      , testCase "contract correctly updates quorum threshold and resets outdated staked token count" $
          integrationalTestExpectation $ checkQuorumThresholdDynamicUpdate
      , testCase "contract correctly updates quorum threshold within max bound" $
          integrationalTestExpectation $ checkQuorumThresholdDynamicUpdateUpperBound
      , testCase "contract correctly updates quorum threshold within min bound" $
          integrationalTestExpectation $ checkQuorumThresholdDynamicUpdateLowerBound
      , testCase "contract correctly stores quorum threshold within the proposal" $
          integrationalTestExpectation $ checkProposalSavesQuorum
      ]
  ]

checkFreezeHistoryTracking :: IntegrationalScenarioM ()
checkFreezeHistoryTracking  = do
  let frozen_scale_value = 2
  let frozen_extra_value = 10

  now <- use isNow

  withOriginated 2 (\(admin:_) -> do
    dodTokenContract <- lOriginate dummyFA2Contract "TokenContract" [] (toMutez 0)
    pure $ mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold (mkQuorumThreshold 10 100)
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress (unTAddress dodTokenContract)
      ! #maxQuorumFraction 19
      ! #changePercent 5
      ! #governanceTotalSupply 100
      ! #customEps mempty
    ) $ \(admin:wallet1:_) baseDao -> let
      proposalMeta1 = ""
      proposalSize1 = fromIntegral . BS.length $ proposalMeta1

      in do
        let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "freeze") (toVal requiredFrozen)

        -- Advance one voting period to a proposing stage.
        rewindTime 11

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta1))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let fh = Map.lookup wallet1 (unBigMap $ sFreezeHistory $ fsStorage storage)
            let expected = AddressFreezeHistory
                  { fhCurrentPeriodNum = 1
                  , fhCurrentUnstaked = 0
                  , fhStaked = requiredFrozen
                  , fhPastUnstaked = 0
                  }
            when (fh /= (Just expected)) $ Left $ CustomTestError "BaseDAO contract did not stake tokens as expected"

        -- Advance two voting periods to another proposing stage.
        rewindTime 21

        tTransfer  (#from .! admin) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "flush") (toVal (1 :: Natural))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            -- We expect all the staked tokens to be unstaked after propoal rejection/accept.
            let fh = Map.lookup wallet1 (unBigMap $ sFreezeHistory $ fsStorage storage)
            let expected = AddressFreezeHistory
                  { fhCurrentPeriodNum = 3
                  , fhCurrentUnstaked = 0
                  , fhStaked = 0
                  , fhPastUnstaked = requiredFrozen
                  }
            when (fh /= (Just expected)) $
              Left $ CustomTestError "BaseDAO contract did not unstake tokens after voting period"

calculateThreshold :: QuorumFraction -> QuorumFraction -> GovernanceTotalSupply -> Integer -> QuorumThreshold -> QuorumThreshold
calculateThreshold (QuorumFraction mcp) (QuorumFraction cp) (GovernanceTotalSupply gts) staked oldQt =
  let
    changePercent = QuorumThreshold . fromIntegral $ percentageToFractionNumerator cp
    participation = fractionToNumerator staked (fromIntegral gts)
    maxQuorumFraction = QuorumThreshold . fromIntegral $ percentageToFractionNumerator mcp
    possibeNewQuorum = addF (subF oldQt (mulF oldQt changePercent)) (mulF participation changePercent)
    one' = mkQuorumThreshold 1 1
    maxNewQuorum = mulF oldQt (addF one' maxQuorumFraction)
    minNewQuorum = divF oldQt (addF one' maxQuorumFraction)
    -- old_quorum - (old_quorum * changePercent) + participation * changePercent
    configMinQuorum = QuorumThreshold $ percentageToFractionNumerator 1
    configMaxQuorum = QuorumThreshold $ percentageToFractionNumerator 99
  in boundQt configMinQuorum configMaxQuorum (boundQt minNewQuorum maxNewQuorum possibeNewQuorum)
  where
    boundQt (QuorumThreshold qmin) (QuorumThreshold qmax) (QuorumThreshold qt) = QuorumThreshold $ min qmax (max qmin qt)
    mulF (QuorumThreshold n1) (QuorumThreshold n2) = QuorumThreshold (div (n1 * n2) (fromIntegral fractionDenominator))
    divF (QuorumThreshold n1) (QuorumThreshold n2) = QuorumThreshold (div (n1 * (fromIntegral fractionDenominator)) n2)
    addF (QuorumThreshold n1) (QuorumThreshold n2) = QuorumThreshold (n1 + n2)
    subF (QuorumThreshold n1) (QuorumThreshold n2) = QuorumThreshold (n1 - n2)
    fractionToNumerator = mkQuorumThreshold

checkQuorumThresholdDynamicUpdate :: IntegrationalScenarioM ()
checkQuorumThresholdDynamicUpdate  = do
  now <- use isNow

  withOriginated 2 (\(admin:_) -> do
    tokenContract <- lOriginate dummyFA2Contract "TokenContract" [] (toMutez 0)
    pure $ mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold (mkQuorumThreshold 3 10)
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress (unTAddress tokenContract)
      ! #maxQuorumFraction 19
      ! #changePercent 5
      ! #governanceTotalSupply 100
      ! #customEps mempty
    ) $ \(_:wallet1:_) baseDao -> do
        let requiredFrozen = (10 :: Natural)

        let proposalMeta1 = ""

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "freeze") (toVal (2 * requiredFrozen))

        rewindTime 11
        -- First proposal period, cycle 0

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta1))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let qs = sQuorumThresholdAtCycle $ fsStorage storage
            let expected = QuorumThresholdAtCycle 1 (mkQuorumThreshold 3 10) 10 -- We don't expect the inital quorum to have updated here
            when (qs /= expected) $
              Left $ CustomTestError ("BaseDAO contract updated quorum threshold unexpectedly" <> (show qs))

        -- skip this proposal period and next voting period to be in next proposal period
        rewindTime 21 -- cycle 2

        let proposalMeta2 = "A"
        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta2))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let qs = sQuorumThresholdAtCycle $ fsStorage storage
            -- We start with quorumThreshold of 3/10, with participation as 10 and governance total supply as 100
            -- participation = 10/100 = 1/10
            -- so possible_new_quorum = 3/10 * (1 - 0.05)  + (1/10 * 5/100)
            -- so 3/10 * (19/20) + (5/1000) = 57/200 + 1/200 = 58/200 = 29/100
            let expected = QuorumThresholdAtCycle 2 (calculateThreshold 19 5 100 10 $ mkQuorumThreshold 3 10) 10
            when (qs /= expected) $
              Left $ CustomTestError ("BaseDAO contract did not update quorum threshold as expected" <> (show qs))

checkQuorumThresholdDynamicUpdateUpperBound :: IntegrationalScenarioM ()
checkQuorumThresholdDynamicUpdateUpperBound  = do
  now <- use isNow

  withOriginated 2 (\(admin:_) -> do
    tokenContract <- lOriginate dummyFA2Contract "TokenContract" [] (toMutez 0)
    pure $ mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold (mkQuorumThreshold 3 10)
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress (unTAddress tokenContract)
      ! #maxQuorumFraction 7
      ! #changePercent 5
      ! #governanceTotalSupply 100
      ! #customEps mempty
    ) $ \(_:wallet1:_) baseDao -> do
        let requiredFrozen = (100 :: Natural)

        let proposalMeta1 = ""

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "freeze") (toVal (2 * requiredFrozen))

        rewindTime 11

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta1))

        rewindTime 21
        let proposalMeta2 = "A"
        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta2))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let qs = sQuorumThresholdAtCycle $ fsStorage storage
            -- We start with quorumThreshold of 3/10, with participation as 100 and governance total supply as 100
            -- participation = 100/100 = 1
            -- so possible_new_quorum = 3/10 * (1 - 0.05)  + (1 * 5/100)
            -- so 3/10 * (19/20) + (5/100) = 57/200 + 1/20 = 67/200 = (0.3350)
            --
            -- min_quorum = (3/10) / 1.07 = 0.28
            -- max_quorum = (3/10) * (107/100) = 321/1000 = 0.321
            let expected = QuorumThresholdAtCycle 2 (calculateThreshold 7 5 100 100 $ mkQuorumThreshold 3 10) 100
            when (qs /= expected) $
              Left $ CustomTestError ("BaseDAO contract did not update quorum threshold as expected")

checkQuorumThresholdDynamicUpdateLowerBound :: IntegrationalScenarioM ()
checkQuorumThresholdDynamicUpdateLowerBound  = do
  now <- use isNow

  withOriginated 2 (\(admin:_) -> do
    tokenContract <- lOriginate dummyFA2Contract "TokenContract" [] (toMutez 0)
    pure $ mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold (mkQuorumThreshold 3 10)
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress (unTAddress tokenContract)
      ! #maxQuorumFraction 19
      ! #changePercent 25
      ! #governanceTotalSupply 100
      ! #customEps mempty
    ) $ \(_:wallet1:_) baseDao -> do
        let requiredFrozen = (100 :: Natural)

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "freeze") (toVal (2 * requiredFrozen))

        -- We raise no proposals here, so participation for calculating next quorum would be zero.

        rewindTime 31
        let proposalMeta2 = "A"
        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta2))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let qs = sQuorumThresholdAtCycle $ fsStorage storage
            -- We start with quorumThreshold of 3/10, with participation as 0 and governance total supply as 100
            -- participation = 100/100 = 1
            -- so possible_new_quorum = 3/10 * (1 - 0.25)  + (0 * 5/100)
            -- so 3/10 * (15/20) = 45/200 = 0.2250
            --
            -- min_quorum = (3/10) / (119/100) = 300/1190 = 0.2521
            -- let expected = QuorumThresholdAtCycle 2 (mkQuorumThreshold 300 1190) 100
            let expected = QuorumThresholdAtCycle 2 (calculateThreshold 19 25 100 0 $ mkQuorumThreshold 3 10) 100
            when (qs /= expected) $
              Left $ CustomTestError ("BaseDAO contract did not update quorum threshold as expected")

checkProposalSavesQuorum :: IntegrationalScenarioM ()
checkProposalSavesQuorum  = do
  now <- use isNow

  withOriginated 2 (\(admin:_) -> do
    tokenContract <- lOriginate dummyFA2Contract "TokenContract" [] (toMutez 0)
    pure $ mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold (mkQuorumThreshold 3 10)
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress (unTAddress tokenContract)
      ! #maxQuorumFraction 19
      ! #changePercent 5
      ! #governanceTotalSupply 100
      ! #customEps mempty
    ) $ \(_:wallet1:_) baseDao -> do
        let requiredFrozen = (10 :: Natural)

        let proposalMeta1 = ""

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "freeze") (toVal (2 * requiredFrozen))

        rewindTime 11
        -- First proposal period, cycle 0

        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal (ProposeParams requiredFrozen proposalMeta1))

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let qs = sQuorumThresholdAtCycle $ fsStorage storage
            let expected = QuorumThresholdAtCycle 1 (mkQuorumThreshold 3 10) 10 -- We don't expect the inital quorum to have updated here
            when (qs /= expected) $
              Left $ CustomTestError ("BaseDAO contract updated quorum threshold unexpectedly")

        -- skip this proposal period and next voting period to be in next proposal period
        rewindTime 21 -- cycle 2

        let proposalMeta2 = "A"
        let proposeParams = ProposeParams requiredFrozen proposalMeta2
        tTransfer  (#from .! wallet1) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "propose") (toVal proposeParams)

        lExpectStorage @FullStorage baseDao $ \storage -> do
            let proposalKey = makeProposalKey proposeParams wallet1
            let proposal = fromMaybe (error "Proposal not found") $ Map.lookup proposalKey $ unBigMap $ sProposals $ fsStorage storage
            -- We start with quorumThreshold of 3/10, with participation as 10 and governance total supply as 100
            -- participation = 10/100 = 1/10
            -- so possible_new_quorum = 3/10 * (1 - 0.05)  + (1/10 * 5/100)
            -- so 3/10 * (19/20) + (5/1000) = 57/200 + 1/200 = 58/200 = 29/100
            let expected = mkQuorumThreshold 290 1000
            when ((plQuorumThreshold proposal) /= expected) $
              Left $ CustomTestError ("BaseDAO contract did not update quorum threshold as expected")
