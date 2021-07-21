-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.Model.BaseDAO.Proposal.QuorumThreshold
  ( updateQuorum
  ) where

import Universum

import Data.Coerce (coerce)

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types

bound :: Integer -> Integer -> Integer -> Integer
bound minQt maxQt qt
  | (qt > maxQt) = maxQt
  | (qt < minQt) = minQt
  | otherwise = qt

stageToCycle :: Natural -> Natural
stageToCycle p = (p + 1) `div` 2

calculateNewQuorumThreshold :: ModelT QuorumThreshold
calculateNewQuorumThreshold = do
  store <- getStore
  config <- getConfig
  let (previousStaked :: Integer) = store & sQuorumThresholdAtCycle & qaStaked & toInteger
      (oldQuorum :: Integer) = store & sQuorumThresholdAtCycle & qaQuorumThreshold & qtNumerator & toInteger

      (governanceTotalSupply :: Integer) = config & cGovernanceTotalSupply & coerce @GovernanceTotalSupply @Natural & toInteger
      QuorumFraction (configMinQt :: Integer) = config & cMinQuorumThreshold
      QuorumFraction (configMaxQt :: Integer) = config & cMaxQuorumThreshold
      QuorumFraction (quorumChange :: Integer) = config & cQuorumChange
      QuorumFraction (maxQuorumChange :: Integer) = config & cMaxQuorumChange

      previousParticipation =
        (previousStaked * fractionDenominator)
        `div` governanceTotalSupply

      possibleNewQuorum =
        oldQuorum +
        ( (quorumChange * (previousParticipation - oldQuorum))
          `div` fractionDenominator
        )

      minNewQuorum =
       ( oldQuorum * fractionDenominator )
       `div` ( maxQuorumChange + fractionDenominator )

      maxNewQuorum =
        ( oldQuorum * (maxQuorumChange + fractionDenominator) )
        `div` fractionDenominator

      newQuorum = possibleNewQuorum
        & bound minNewQuorum maxNewQuorum
        & bound configMinQt configMaxQt

  if newQuorum < 0 then
    error "BAD_STATE: newQuorum is negative."
  else
    pure $ QuorumThreshold $ fromIntegral newQuorum

updateQuorum :: Natural -> ModelT ()
updateQuorum currentLevel = do
  store <- getStore
  let currentCycle = stageToCycle currentLevel
  when (currentCycle > (store & sQuorumThresholdAtCycle & qaLastUpdatedCycle)) $ do
    quorumThreshold <- calculateNewQuorumThreshold
    let newQuorumThresholdAtCycle = QuorumThresholdAtCycle
          { qaQuorumThreshold = quorumThreshold
          , qaLastUpdatedCycle = currentCycle
          , qaStaked = 0
          }
    modifyStore $ \s -> pure $ s { sQuorumThresholdAtCycle = newQuorumThresholdAtCycle}