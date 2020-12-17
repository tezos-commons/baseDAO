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

config :: forall ce pm. (IsoValue pm) => DAO.Config ce pm
config = DAO.defaultConfig
  { DAO.cProposalCheck = do
      dip drop
      toFieldNamed #ppFrozenToken
      -- Submitted proposal must freeze more than 10 tokens
      push (10 :: Natural)
      toNamed #requireValue

      if #requireValue <=. #ppFrozenToken then
        push True
      else
        push False
  }

configWithRejectedProposal :: forall ce pm. (IsoValue pm) => DAO.Config ce pm
configWithRejectedProposal = DAO.defaultConfig
  { DAO.cRejectedProposalReturnValue = do
      dip drop
      -- divide frozen value by half
      toField #pProposerFrozenToken
      push (2 :: Natural)
      swap
      ediv
      ifSome car $
        push (0 :: Natural)
      toNamed #slash_amount
  }

badRejectedValueConfig :: forall ce pm. DAO.Config ce pm
badRejectedValueConfig = DAO.defaultConfig
  { DAO.cRejectedProposalReturnValue = do
      drop; drop; push (9999 :: Natural)
      toNamed #slash_amount
  }

decisionLambdaConfig :: forall ce pm. (IsoValue pm) => DAO.Config ce pm
decisionLambdaConfig = config
  { DAO.cDecisionLambda = decisionLambda
  }
  where
    decisionLambda :: forall s store. (DAO.StorageC store ce pm, DAO.HasFuncContext s store)
      => (DAO.Proposal pm) : store : s
      :-> List Operation : store : s
    decisionLambda = do
      toField #pProposer
      dip $ do
        push (DAO.unfrozenTokenId, 10 :: Natural)
      dig @2
      stackType @(store : Address : (FA2.TokenId, Natural) : s)
      DAO.callCachedFunc creditTo
      nil
