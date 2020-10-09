-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Common
  ( withOriginated
  ) where

import Universum

import Lorentz
import Lorentz.Contracts.BaseDAO (baseDaoContract)
import Lorentz.Contracts.BaseDAO.Types
import Morley.Nettest

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> Storage)
  -> ([Address] -> TAddress Parameter -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  baseDao <- originate $ OriginateData
    { odFrom = nettestAddress
    , odName = "BaseDAO Test Contract"
    , odBalance = zeroMutez
    , odStorage = storageFn addresses
    , odContract = baseDaoContract
    }
  tests addresses baseDao
