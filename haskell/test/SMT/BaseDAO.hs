-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}

module SMT.BaseDAO
  ( hprop_SMT
  ) where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import Hedgehog
import Lorentz hiding (div, fromInteger, now, (>>))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Ligo.BaseDAO.Types
import SMT.Common.Run
import SMT.Common.Types
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common (ContractType(..), makeProposalKey, metadataSize)
import Test.Ligo.BaseDAO.Proposal.Config

hprop_SMT :: Property
hprop_SMT =
  let
    option = SmtOption
      { soMkPropose = genPropose
      , soMkCustomCalls = pure []
      , soModifyFs = addBaseDaoConfig
      , soContractType = BaseDaoContract

      , soProposalCheck = \(ProposeParams{..}, _) -> do
        -- Implemented from `proposalFrozenTokensMinBound 10`
        let minTokens = 10
        unless (minTokens <= ppFrozenToken) $
          throwError FAIL_PROPOSAL_CHECK

      , soRejectedProposalSlashValue = \(Proposal{..}, _) -> do
        -- Implemented from `divideOnRejectionBy 2`
        let divisor = 2
        pure $ plProposerFrozenToken `div` divisor

      , soDecisionLambda = \DecisionLambdaInput'{..} -> do
        pure $ ([], diExtra, Nothing)

      , soCustomEps = \_ -> pure ()
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT @'TestDynamic option

addBaseDaoConfig :: FullStorageSkeleton (VariantToExtra 'TestDynamic) -> FullStorageSkeleton (VariantToExtra 'TestDynamic)
addBaseDaoConfig fs = fs
  { fsStorage = (fsStorage fs) { sExtra = ce }
  }
  where
    ce = fillConfig dummyDecisionLambda $
      fillConfig (divideOnRejectionBy 2) $
        fillConfig (proposalFrozenTokensMinBound 10) dynRecUnsafe

genPropose :: MkGenPropose 'TestDynamic
genPropose senderInput delegate1 invalidFrom = do
  from <- Gen.element [senderInput, invalidFrom, delegate1]
  metadata <- Gen.integral (Range.constant 1 100)
  let proposalMeta = lPackValueRaw @Natural metadata
  let
      metaSize = metadataSize proposalMeta
      param = ProposeParams
          { ppFrom = from
          , ppFrozenToken = metaSize
          , ppProposalMetadata = proposalMeta
          }
      proposalKey = makeProposalKey param

  pure $ (\_ _ -> (XtzAllowed $ ConcreteEp $ Propose param, metaSize, proposalKey))
