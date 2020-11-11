-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.BaseDAO.ProposalConfig
  ( config
  , configWithRejectedProposal
  , badRejectedValueConfig
  , decisionLambdaConfig
  ) where

import Lorentz

import Lorentz.Contracts.BaseDAO.Token.FA2 (creditTo)
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

config :: forall pm. (IsoValue pm) => DAO.Config pm
config = DAO.defaultConfig
  { DAO.cProposalCheck = do
      toFieldNamed #ppFrozenToken
      -- Submitted proposal must freeze more than 10 tokens
      push (10 :: Natural)
      toNamed #requireValue

      if #requireValue <=. #ppFrozenToken then
        push True
      else
        push False
  }

configWithRejectedProposal :: forall pm. (IsoValue pm) => DAO.Config pm
configWithRejectedProposal = DAO.defaultConfig
  { DAO.cRejectedProposalReturnValue = do
      -- ^ divide frozen value by half
      toField #pProposerFrozenToken
      push (2 :: Natural)
      swap
      ediv
      ifSome car $
        push (0 :: Natural)
      toNamed #slash_amount
  }

badRejectedValueConfig :: forall pm. DAO.Config pm
badRejectedValueConfig = DAO.defaultConfig
  { DAO.cRejectedProposalReturnValue = do
      drop; push (9999 :: Natural)
      toNamed #slash_amount
  }

decisionLambdaConfig :: forall pm. IsoValue pm => DAO.Config pm
decisionLambdaConfig = config
  { DAO.cDecisionLambda = decisionLambda
  }
  where
    decisionLambda :: forall s store. (DAO.StorageC store pm, DAO.HasFuncContext s store)
      => (DAO.Proposal pm) : store : s
      :-> (List Operation, store) : s
    decisionLambda = do
      toField #pProposer
      dip $ do
        push (DAO.unfrozenTokenId, 10 :: Natural)
      dig @2
      stackType @(store : Address : (FA2.TokenId, Natural) : s)
      DAO.callCachedFunc creditTo
      nil; pair
