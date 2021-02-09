-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Universum

import Test.Tasty (TestTree)

import BaseDAO.ShareTest.Proposal
import Ligo.BaseDAO.Types (dynRecUnsafe)
import Test.Ligo.BaseDAO.Common

test_BaseDAO_Proposal :: TestTree
test_BaseDAO_Proposal = mkBaseDaoProposalTests $
  withDefaultStartup . originateLigoDaoWithConfigDesc dynRecUnsafe
