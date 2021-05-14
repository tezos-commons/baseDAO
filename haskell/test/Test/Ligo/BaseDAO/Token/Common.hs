-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains FA2 tests for testing the Ligo contract.
module Test.Ligo.BaseDAO.Token.Common
  ( originateWithCustomToken
  , transfer
  , assertBalanceOf

  -- * Re-exports
  , DaoOriginateData(..)
  , frozenTokens
  , unfrozenTokens
  , unfrozenTokens1
  , unknownTokens
  ) where

import Universum

import Lorentz (Address, TAddress, toAddress, toVal)
import Lorentz.Test (contractConsumer)
import Morley.Nettest hiding (transfer)

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common

originateWithCustomToken :: MonadNettest caps base m => OriginateFn m
originateWithCustomToken =
  originateLigoDaoWithBalance dynRecUnsafe defaultConfig
      (\dodOwner1 dodOwner2 ->
          [ ((dodOwner1, frozenTokens), 100)
          , ((dodOwner2, frozenTokens), 100)
          , ((dodOwner1, unfrozenTokens), 1000)
          , ((dodOwner2, unfrozenTokens), 1000)
          , ((dodOwner1, unfrozenTokens1), 1000)
          ]
      )

transfer
  :: MonadNettest caps base m
  => Natural -> FA2.TokenId -> Address -> Address -> TAddress Parameter -> m ()
transfer amount tokenId from to dao =
  call dao (Call @"Transfer") param
  where
    param = [ FA2.TransferItem
        { tiFrom = from
        , tiTxs = [ FA2.TransferDestination
            { tdTo = to
            , tdTokenId = tokenId
            , tdAmount = amount
            } ]
        } ]

assertBalanceOf
  :: MonadNettest caps base m
  => Address -> Natural -> FA2.TokenId -> TAddress Parameter -> m ()
assertBalanceOf tokenOwner expectedBalance tokenId dao = do
  consumer <- originateSimple "consumer" [] contractConsumer
  call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = tokenOwner
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((tokenOwner, tokenId), expectedBalance :: Natural)]] )
