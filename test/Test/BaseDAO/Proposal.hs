-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz.Empty
import Test.Tasty (TestTree)

import BaseDAO.ShareTest.Common
import BaseDAO.ShareTest.Proposal

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Proposal :: TestTree
test_BaseDAO_Proposal =
  mkBaseDaoProposalTests
    (originateBaseDaoWithConfig @Integer @Empty ())
