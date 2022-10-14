-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

-- | Contains tests on @propose@ entrypoint logic for testing the  behaviour
-- associated with quorum threshold and its dynamic updates.
module Test.Ligo.BaseDAO.Proposal.Quorum
  ( proposalIsRejectedIfNoQuorum
  , proposalSucceedsIfUpVotesGtDownvotesAndQuorum
  , checkQuorumThresholdDynamicUpdate
  , checkQuorumThresholdDynamicUpdateUpperBound
  , checkQuorumThresholdDynamicUpdateLowerBound
  , checkProposalSavesQuorum
  ) where

import Universum

import Morley.Tezos.Address
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

calculateThreshold :: Natural -> Natural -> GovernanceTotalSupply -> Integer -> QuorumThreshold -> QuorumThreshold
calculateThreshold mcp cp (GovernanceTotalSupply gts) staked oldQt =
  let
    changePercent = QuorumThreshold $ percentageToFractionNumerator cp
    participation = fractionToNumerator staked (fromIntegral gts)
    maxChangePercent = QuorumThreshold $ percentageToFractionNumerator mcp
    possibeNewQuorum = addF (subF oldQt (mulF oldQt changePercent)) (mulF participation changePercent)
    one' = mkQuorumThreshold 1 1
    maxNewQuorum = mulF oldQt (addF one' maxChangePercent)
    minNewQuorum = divF oldQt (addF one' maxChangePercent)
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

checkQuorumThresholdDynamicUpdate
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
checkQuorumThresholdDynamicUpdate originateFn = do
  DaoOriginateData{..} <- originateFn
      ((ConfigDesc $ (#changePercent :! (5 :: Natural)))
      >>- (ConfigDesc $ (#maxChangePercent :! (19 :: Natural)))
      >>- (ConfigDesc $ (#governanceTotalSupply :! (100 :: Natural)))
      >>- (ConfigDesc $ Period 20)
      >>- (ConfigDesc configConsts)
      ) (mkQuorumThreshold 3 10)
  let proposer = dodOwner1
  let requiredFrozen = (10 :: Natural)
  let proposalMeta1 = ""

  withSender proposer $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! (2 * requiredFrozen))

  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  -- First proposal period, cycle 0

  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta1)

  quorumThresholdActual_ <- getQtAtCycle dodDao
  let quorumThresholdExpected_ = QuorumThresholdAtCycle (mkQuorumThreshold 3 10) 1 10
  assert (quorumThresholdActual_ == quorumThresholdExpected_) "Unexpected quorumThreshold update"

  -- skip this proposal period and next voting period to be in next proposal period
  advanceToLevel (startLevel + 3 * dodPeriod + 1)

  let proposalMeta2 = "A"
  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta2)

  quorumThresholdActual <- getQtAtCycle dodDao
  -- We start with quorumThreshold of 3/10, with participation as 10 and governance total supply as 100
  -- participation = 10/100 = 1/10
  -- so possible_new_quorum = 3/10 * (1 - 0.05)  + (1/10 * 5/100)
  -- so 3/10 * (19/20) + (5/1000) = 57/200 + 1/200 = 58/200 = 29/100
  let quorumThresholdExpected = QuorumThresholdAtCycle (calculateThreshold 19 5 100 10 $ mkQuorumThreshold 3 10) 2 10
  assert (quorumThresholdActual == quorumThresholdExpected) "Unexpected quorumThreshold after update"

checkQuorumThresholdDynamicUpdateUpperBound
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
checkQuorumThresholdDynamicUpdateUpperBound originateFn = do
  DaoOriginateData{..} <- originateFn
      ((ConfigDesc $ (#changePercent :! (5 :: Natural)))
      >>- (ConfigDesc $ (#maxChangePercent :! (7 :: Natural)))
      >>- (ConfigDesc $ (#governanceTotalSupply :! (100 :: Natural)))
      >>- (ConfigDesc $ Period 20)
      >>- (ConfigDesc configConsts)
      ) (mkQuorumThreshold 3 10)
  let proposer = dodOwner1
  let dao = dodDao
  let requiredFrozen = (100 :: Natural)
  let proposalMeta1 = ""

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! (2 * requiredFrozen))

  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  -- First proposal period, cycle 0

  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta1)

  -- skip this proposal period and next voting period to be in next proposal period
  advanceToLevel (startLevel + 3*dodPeriod + 1)

  let proposalMeta2 = "A"
  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta2)

  quorumThresholdActual <- getQtAtCycle dodDao
  -- We start with quorumThreshold of 3/10, with participation as 100 and governance total supply as 100
  -- participation = 100/100 = 1
  -- so possible_new_quorum = 3/10 * (1 - 0.05)  + (1 * 5/100)
  -- so 3/10 * (19/20) + (5/100) = 57/200 + 1/20 = 67/200 = (0.3350)
  --
  -- min_quorum = (3/10) / 1.07 = 0.28
  -- max_quorum = (3/10) * (107/100) = 321/1000 = 0.321
  let quorumThresholdExpected = QuorumThresholdAtCycle (calculateThreshold 7 5 100 100 $ mkQuorumThreshold 3 10) 2 100
  assert (quorumThresholdActual == quorumThresholdExpected) "Unexpected quorumThreshold after update"

checkQuorumThresholdDynamicUpdateLowerBound
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
checkQuorumThresholdDynamicUpdateLowerBound originateFn = do
  DaoOriginateData{..} <- originateFn
      ((ConfigDesc $ (#changePercent :! (25 :: Natural)))
      >>- (ConfigDesc $ (#maxChangePercent :! (19 :: Natural)))
      >>- (ConfigDesc $ (#governanceTotalSupply :! (100 :: Natural)))
      >>- (ConfigDesc $ Period 20)
      >>- (ConfigDesc configConsts)
      ) (mkQuorumThreshold 3 10)
  let proposer = dodOwner1
  let dao = dodDao
  let requiredFrozen = (100 :: Natural)
  let proposalMeta1 = ""

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! (2 * requiredFrozen))

  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  -- First proposal period, cycle 0

  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta1)

  -- skip this proposal period and next voting period to be in next proposal period
  advanceToLevel (startLevel + 3 * dodPeriod + 1)

  let proposalMeta2 = "A"
  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta2)

  quorumThresholdActual <- getQtAtCycle dodDao
  -- We start with quorumThreshold of 3/10, with participation as 0 and governance total supply as 100
  -- participation = 100/100 = 1
  -- so possible_new_quorum = 3/10 * (1 - 0.25)  + (0 * 5/100)
  -- so 3/10 * (15/20) = 45/200 = 0.2250
  --
  -- min_quorum = (3/10) / (119/100) = 300/1190 = 0.2521
  -- let expected = QuorumThresholdAtCycle 2 (mkQuorumThreshold 300 1190) 100
  let quorumThresholdExpected = QuorumThresholdAtCycle (calculateThreshold 19 25 100 100 $ mkQuorumThreshold 3 10) 2 100
  assert (quorumThresholdActual == quorumThresholdExpected) "Unexpected quorumThreshold after update"

checkProposalSavesQuorum
  :: forall caps m. (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
checkProposalSavesQuorum originateFn = do
  DaoOriginateData{..} <- originateFn
      ((ConfigDesc $ (#changePercent :! (5 :: Natural)))
      >>- (ConfigDesc $ (#maxChangePercent :! (19 :: Natural)))
      >>- (ConfigDesc $ (#governanceTotalSupply :! (100 :: Natural)))
      >>- (ConfigDesc $ Period 20)
      >>- (ConfigDesc configConsts)
      ) (mkQuorumThreshold 3 10)
  let proposer = dodOwner1
  let dao = dodDao
  let requiredFrozen = (10 :: Natural)
  let proposalMeta1 = ""

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! (2 * requiredFrozen))

  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  -- First proposal period, cycle 0

  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") (ProposeParams (MkAddress proposer) requiredFrozen proposalMeta1)

  -- skip this proposal period and next voting period to be in next proposal period
  advanceToLevel (startLevel + 3*dodPeriod + 1)

  let proposalMeta2 = "A"
  let proposeParams = ProposeParams (MkAddress proposer) requiredFrozen proposalMeta2
  withSender proposer $
    transfer dodDao $ calling (ep @"Propose") proposeParams

  let proposalKey = makeProposalKey proposeParams
  proposal <- fromMaybe (error "Proposal not found") <$> getProposal dodDao proposalKey
  -- We start with quorumThreshold of 3/10, with participation as 10 and governance total supply as 100
  -- participation = 10/100 = 1/10
  -- so possible_new_quorum = 3/10 * (1 - 0.05)  + (1/10 * 5/100)
  -- so 3/10 * (19/20) + (5/1000) = 57/200 + 1/200 = 58/200 = 29/100
  let expected = mkQuorumThreshold 290 1000
  assert ((plQuorumThreshold proposal) == expected) $
    "BaseDAO contract did not store quorum threshold in proposal as expected"

proposalIsRejectedIfNoQuorum
  :: (MonadCleveland caps m, HasCallStack)
  => m ()
proposalIsRejectedIfNoQuorum = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc @'Base ()
      ((ConfigDesc $ Period 60)
      >>- (ConfigDesc (FixedFee 42))
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 1800 })
      ) (mkQuorumThreshold 1 20)
  let proposer = dodOwner1
  let voter = dodOwner2
  let dao = dodDao
  let admin = dodAdmin

  withSender voter $
    transfer dao$ calling (ep @"Freeze") (#amount :! 28)

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! 42)

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 proposer dao
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  let vote_ =
        NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 3
            -- The minimum votes that is required to pass is 80 * 1/20  = 4.
          , vProposalKey = key1
          , vFrom = MkAddress voter
          }
  withSender voter $
    transfer dao$ calling (ep @"Vote") [vote_]

  let expectedFrozen = 42 + 10
  checkBalance dao proposer expectedFrozen

  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod)
  withSender admin $ transfer dao $ calling (ep @"Flush") 100

  checkBalance dao proposer 9 -- We expect 42 + 1 tokens to have burned

proposalSucceedsIfUpVotesGtDownvotesAndQuorum
  :: (MonadCleveland caps m, HasCallStack)
  => m ()
proposalSucceedsIfUpVotesGtDownvotesAndQuorum = do
  DaoOriginateData{..} <-
    originateLigoDaoWithConfigDesc @'Base ()
      (testConfig
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc (FixedFee 42))
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 1800 })
      ) $ mkQuorumThreshold 1 20
  let proposer = dodOwner1
  let voter = dodOwner2
  let dao = dodDao
  let admin = dodAdmin

  withSender voter $
    transfer dao$ calling (ep @"Freeze") (#amount :! 28)

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! 42)

  withSender proposer $
    transfer dao$ calling (ep @"Freeze") (#amount :! 10)

  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel dodDao
  advanceToLevel (startLevel + dodPeriod)
  key1 <- createSampleProposal 1 proposer dao
  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  let vote_ =
        NoPermit VoteParam
          { vVoteType = True
          , vVoteAmount = 4
            -- The minimum votes that is required to pass is 80 * 1/20  = 4.
          , vProposalKey = key1
          , vFrom = MkAddress voter
          }
  withSender voter $
    transfer dao$ calling (ep @"Vote") [vote_]

  let expectedFrozen = 42 + 10
  checkBalance dao proposer expectedFrozen

  proposalStart <- getProposalStartLevel dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod)
  withSender admin $ transfer dao $ calling (ep @"Flush") 100

  checkBalance dao proposer 52
