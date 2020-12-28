-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.RegistryDAO.Doc
   ( registryDaoDoc
   ) where

import Lorentz

import Util.Markdown

registryDaoDoc :: Markdown
registryDaoDoc = [md|
  Registry DAO is a decentralized key-value storage of arbitrary data.
  It's parameterized by two types: `k` (for key) and `v` (for value).
  In UI, these `k` and `v` parameters both will be `bytes` and will be converted to application-level types externally.
  Application-level types can be stored as part of the contract's metadata as specified in TZIP-16.
  |]