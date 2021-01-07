-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-16 metadata.
module Lorentz.Contracts.BaseDAO.TZIP16Metadata
  ( knownBaseDAOMetadata

    -- * Views
  , getBalanceView
  , allTokensView
  , isOperatorView
  , tokenMetadataView
  ) where

import qualified Data.Map as Map
import Data.Version (showVersion)

import Lorentz hiding (View)
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Metadata

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Lorentz.Contracts.BaseDAO.Token.FA2 (convertOperatorParam)
import Lorentz.Contracts.BaseDAO.Types
import qualified Paths_baseDAO as Paths

-- For metadata storage does not matter
type MetadataStorage = Storage () ()

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
knownBaseDAOMetadata :: Metadata (ToT MetadataStorage)
knownBaseDAOMetadata = mconcat
  [ version . fromString $ showVersion Paths.version
  , license $ License "MIT" Nothing
  , authors [Author "Serokell", Author "Tocqueville Group"]
  , homepage "https://github.com/tqtezos/baseDAO"
  , interfaces [tzip 12, tzip 17]
  , views baseDAOViews
  ]

------------------------------------------------------------------------
-- Off-chain views
------------------------------------------------------------------------

type DaoView = View (ToT MetadataStorage)

baseDAOViews :: [View $ ToT MetadataStorage]
baseDAOViews =
  [ getBalanceView
  , allTokensView
  , isOperatorView
    -- total_supply is skipped since the necessary info is not tracked
    -- TODO [#126]: implement it some day
  , tokenMetadataView

  , permitsCounterView
  ]

-- FA2
------------------------------------------------------------------------

getBalanceView :: DaoView
getBalanceView = View
  { vName = "get_balance"
  , vDescription = Just
      "Get balance of an address according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @MetadataStorage @Natural Nothing [] $
            WithParam @FA2.BalanceRequestItem $ do
              getField #briOwner; dip (toField #briTokenId); pair
              stGet #sLedger
              fromOption 0
      ]
  }

allTokensView :: DaoView
allTokensView = View
  { vName = "all_tokens"
  , vDescription = Just
      "Get all supported tokens according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @MetadataStorage @[FA2.TokenId] Nothing [] $
            WithoutParam $ do
              drop @MetadataStorage
              push allTokenIds
      ]
  }

isOperatorView :: DaoView
isOperatorView = View
  { vName = "is_operator"
  , vDescription = Just
      "Checks whether given address is allowed to transfer given tokens \
      \that belong to given owner - according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @MetadataStorage @Bool Nothing [] $
            WithParam @FA2.OperatorParam $ do
              convertOperatorParam
              pair
              stMem #sOperators
      ]
  }

tokenMetadataView :: DaoView
tokenMetadataView = View
  { vName = "token_metadata"
  , vDescription = Just
      "Returns metadata for given token according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView
           @MetadataStorage
           @(FA2.TokenId, FA2.TokenMetadata)
           Nothing [] $
            WithParam @FA2.TokenId $ do
              dip $ do
                drop @MetadataStorage
                push $ Map.fromList
                  [ (frozenTokenId, cFrozenTokenMetadata config)
                  , (unfrozenTokenId, cUnfrozenTokenMetadata config)
                  ]
              dup
              dip $ do
                get
                ifSome nop $ failCustom_ #fA2_TOKEN_UNDEFINED
              pair
      ]
  }
  where
    -- TODO: what to do here?
    -- Accept token metadatas in CLI?
    config = defaultConfig



-- BaseDAO-specific
------------------------------------------------------------------------

permitsCounterView :: DaoView
permitsCounterView = View
  { vName = "GetCounter"
  , vDescription = Just
      "Returns the next counter value with which a permit should be created."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkSimpleMichelsonStorageView @MetadataStorage . WithoutParam $
            stToField #sPermitsCounter
      ]
  }
