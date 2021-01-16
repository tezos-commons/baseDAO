-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-16 metadata.
module Lorentz.Contracts.BaseDAO.TZIP16Metadata
  ( MetadataConfig (..)
  , defaultMetadataConfig
  , knownBaseDAOMetadata

    -- * Views
  , getBalanceView
  , allTokensView
  , isOperatorView
  , tokenMetadataView
  ) where

import qualified Universum as U

import qualified Data.Map as Map
import Data.Version (showVersion)
import Fmt (pretty)

import Lorentz hiding (View)
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Metadata

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Lorentz.Contracts.BaseDAO.Token.FA2 (convertOperatorParam)
import Lorentz.Contracts.BaseDAO.Types
import qualified Paths_baseDAO as Paths

-- | All the information for instantiating metadata.
--
-- This includes pieces defined by user (if any) like some constants for
-- off-chain views, as well as abstract storage accessors suitable for
-- Lorentz/LIGO versions of the contract.
data MetadataConfig store =
  StorageContains store
    [ "sLedger" := LedgerKey ~> LedgerValue
    , "sOperators" := Operator ~> ()
    , "sPermitsCounter" := Nonce
    ]
  =>
  MetadataConfig
  { mcFrozenTokenMetadata :: FA2.TokenMetadata
  , mcUnfrozenTokenMetadata :: FA2.TokenMetadata

    -- Haskell-world accessors
  , mcOperatorsL :: U.Lens' store Operators
  }

-- | Default values for config.
-- Should we in fact fill it from CLI?
defaultMetadataConfig :: MetadataConfig (Storage () ())
defaultMetadataConfig =
  let Config{..} = defaultConfig
  in MetadataConfig
    { mcFrozenTokenMetadata = cFrozenTokenMetadata
    , mcUnfrozenTokenMetadata = cUnfrozenTokenMetadata

    , mcOperatorsL = sOperatorsL
    }

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
knownBaseDAOMetadata :: MetadataConfig store -> Metadata (ToT store)
knownBaseDAOMetadata config = mconcat
  [ version . fromString $ showVersion Paths.version
  , license $ License "MIT" Nothing
  , authors [Author "Serokell", Author "Tocqueville Group"]
  , homepage "https://github.com/tqtezos/baseDAO"
  , interfaces [tzip 12, tzip 17]
  , views (baseDAOViews config)
  ]

------------------------------------------------------------------------
-- Off-chain views
------------------------------------------------------------------------

-- | A version of 'compileViewCode' that is simpler to use.
--
-- We cannot use 'compileViewCodeTH' easily because this way passing config
-- won't work.
compileViewCode_ :: ViewCode st ret -> CompiledViewCode st ret
compileViewCode_ = U.either (U.error . pretty) U.id . compileViewCode

baseDAOViews :: MetadataConfig store -> [View $ ToT store]
baseDAOViews = U.sequence
  [ getBalanceView
  , allTokensView
  , isOperatorView
    -- total_supply is skipped since the necessary info is not tracked
    -- TODO [#126]: implement it some day
  , tokenMetadataView

  , permitsCounterView
  ]

type DaoView store = MetadataConfig store -> View (ToT store)

-- FA2
------------------------------------------------------------------------

getBalanceView :: forall store. DaoView store
getBalanceView MetadataConfig{} = View
  { vName = "get_balance"
  , vDescription = Just
      "Get balance of an address according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @Natural Nothing [] $
            compileViewCode_ $ WithParam @FA2.BalanceRequestItem $ do
              getField #briOwner; dip (toField #briTokenId); pair
              stGet #sLedger
              fromOption 0
      ]
  }

allTokensView :: forall store. DaoView store
allTokensView _ = View
  { vName = "all_tokens"
  , vDescription = Just
      "Get all supported tokens according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @[FA2.TokenId] Nothing [] $
            compileViewCode_ $ WithoutParam $ do
              drop @store
              push allTokenIds
      ]
  }

isOperatorView :: forall store. DaoView store
isOperatorView MetadataConfig{} = View
  { vName = "is_operator"
  , vDescription = Just
      "Checks whether given address is allowed to transfer given tokens \
      \that belong to given owner - according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @Bool Nothing [] $
            compileViewCode_ $ WithParam @FA2.OperatorParam $ do
              convertOperatorParam
              pair
              stMem #sOperators
      ]
  }

tokenMetadataView :: forall store. DaoView store
tokenMetadataView MetadataConfig{..} = View
  { vName = "token_metadata"
  , vDescription = Just
      "Returns metadata for given token according to TZIP-12."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView
           @store
           @(FA2.TokenId, FA2.TokenMetadata)
           Nothing [] $
            compileViewCode_ $ WithParam @FA2.TokenId $ do
              dip $ do
                drop @store
                push $ Map.fromList
                  [ (frozenTokenId, mcFrozenTokenMetadata)
                  , (unfrozenTokenId, mcUnfrozenTokenMetadata)
                  ]
              dup
              dip $ do
                get
                ifSome nop $ failCustom_ #fA2_TOKEN_UNDEFINED
              pair
      ]
  }

-- BaseDAO-specific
------------------------------------------------------------------------

permitsCounterView :: forall store. DaoView store
permitsCounterView MetadataConfig{} = View
  { vName = "GetCounter"
  , vDescription = Just
      "Returns the next counter value with which a permit should be created."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkSimpleMichelsonStorageView @store $
            compileViewCode_ $ WithoutParam $
              stToField #sPermitsCounter
      ]
  }
