-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Simplest DAOs on top of BaseDAO.
module Lorentz.Contracts.TrivialDAO
  ( trivialConfig
  , trivialDaoContract
  ) where

import Lorentz

import Lorentz.Contracts.BaseDAO

-- | The default configuration that is specialized to a contract
-- without any user-defined metadata and extras.
trivialConfig :: Config () () Empty
trivialConfig = defaultConfig

trivialDaoContract :: DaoContract () () Empty
trivialDaoContract = baseDaoContract trivialConfig
