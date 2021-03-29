-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.TreasuryDAO.Doc
   ( treasuryDaoDoc
   ) where

import Lorentz

import Util.Markdown

treasuryDaoDoc :: Markdown
treasuryDaoDoc = [md|
   Treasury DAO is a smart contract that holds XTZ and FA2 tokens and lets its users
   decide how to spend its XTZ and tokens.
 |]