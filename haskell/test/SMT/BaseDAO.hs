-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}

module SMT.BaseDAO
  ( hprop_SMT
  ) where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import Hedgehog
import Lorentz hiding (now, (>>))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import SMT.Common.Run
import SMT.Model.BaseDAO.Types
import SMT.Common.Types
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common (makeProposalKey, metadataSize)


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

      , soDecisionLambda = \DecisionLambdaInput{..} -> do
        pure $ ([], diExtra, Nothing)

      , soCustomEps = mempty
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT option

addBaseDaoConfig :: FullStorage -> FullStorage
addBaseDaoConfig fs = fs
  { fsConfig = (fs & fsConfig)
      { cProposalCheck =
        -- Implemented from `proposalFrozenTokensMinBound 10`
          dip drop
        # toFieldNamed #ppFrozenToken
        # push 10
        # toNamed #requireValue
        # (if #requireValue <=. #ppFrozenToken then
              push ()
          else
              push ""
            # failCustom #fAIL_PROPOSAL_CHECK
          )
      , cRejectedProposalSlashValue =
        -- Implemented from `divideOnRejectionBy 2`
          dip drop
        # toField #plProposerFrozenToken
        # toNamed #proposerFrozenToken
        # fromNamed #proposerFrozenToken
        # push (2 :: Natural)
        # swap
        # ediv
        # ( ifSome car $
              push (0 :: Natural)
          )
        # toNamed #slash_amount
      , cDecisionLambda =
          toField #diExtra
        # nil
        # swap
        # dip (push Nothing)
        # constructStack @(DecisionLambdaOutput BigMap)
      }
  }

genPropose :: MkGenPropose
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

  pure $ (\_ _ -> (XtzAllowed $ Propose param, metaSize, proposalKey))
