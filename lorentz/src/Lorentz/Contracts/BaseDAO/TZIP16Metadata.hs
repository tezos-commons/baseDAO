-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-016 metadata.
module Lorentz.Contracts.BaseDAO.TZIP16Metadata
  ( MetadataConfig (..)
  , MetadataSettings (..)
  , defaultMetadataConfig
  , mkMetadataSettings
  , knownBaseDAOMetadata

    -- * Views
  , getBalanceView
  , allTokensView
  , isOperatorView
  , tokenMetadataView
  , getTotalSupplyView
  ) where

import qualified Universum as U

import Data.Version (showVersion)

import Lorentz hiding (View)
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Metadata

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import Lorentz.Contracts.BaseDAO.Token.FA2 (convertOperatorParam)
import Lorentz.Contracts.BaseDAO.Types
import qualified Paths_baseDAO as Paths

-- | Piece of metadata defined by user.
data MetadataConfig = MetadataConfig
  { mcFrozenTokenMetadata :: FA2.TokenMetadata
  , mcUnfrozenTokenMetadata :: FA2.TokenMetadata
  }

-- | All the information for instantiating metadata.
--
-- This includes pieces defined by user (if any) like some constants for
-- off-chain views, as well as abstract storage accessors suitable for
-- Lorentz/LIGO versions of the contract.
data MetadataSettings store =
  StorageContains store
    [ "sLedger" := LedgerKey ~> LedgerValue
    , "sOperators" := Operator ~> ()
    , "sPermitsCounter" := Nonce
    , "sTotalSupply" := FA2.TokenId ~> Natural
    , "sUnfrozenTokenId" := FA2.TokenId
    , "sFrozenTokenId" := FA2.TokenId
    ]
  =>
  MetadataSettings
  { msConfig :: MetadataConfig

    -- Haskell-world accessors
  , mcOperatorsL :: U.Lens' store Operators
  }

-- | Default values for config.
defaultMetadataConfig :: MetadataConfig
defaultMetadataConfig =
  MetadataConfig
    { mcFrozenTokenMetadata =
        FA2.mkTokenMetadata "frozen_token" "Frozen Token" "8"
    , mcUnfrozenTokenMetadata =
        FA2.mkTokenMetadata "unfrozen_token" "Unfrozen Token" "8"
    }

-- | Construct settings for Lorentz version of the contract.
mkMetadataSettings :: (IsoValue ce, IsoValue pm) => MetadataConfig -> MetadataSettings (Storage ce pm)
mkMetadataSettings msConfig = MetadataSettings
  { msConfig
  , mcOperatorsL = sOperatorsL
  }

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
knownBaseDAOMetadata :: MetadataSettings store -> Metadata (ToT store)
knownBaseDAOMetadata settings = mconcat
  [ version . fromString $ showVersion Paths.version
  , license $ License "MIT" Nothing
  , authors [Author "Serokell", Author "Tocqueville Group"]
  , homepage "https://github.com/tqtezos/baseDAO"
  , interfaces [tzip 12, tzip 17]
  , views (baseDAOViews settings)
  ]

------------------------------------------------------------------------
-- Off-chain views
------------------------------------------------------------------------

baseDAOViews :: MetadataSettings store -> [View $ ToT store]
baseDAOViews = U.sequence
  [ getBalanceView
  , allTokensView
  , isOperatorView
  , tokenMetadataView
  , getTotalSupplyView

  , permitsCounterView
  ]

type DaoView store = MetadataSettings store -> View (ToT store)

-- FA2
------------------------------------------------------------------------

getBalanceView :: forall store. DaoView store
getBalanceView MetadataSettings{} = View
  { vName = "get_balance"
  , vDescription = Just
      "Get balance of an address according to TZIP-012."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @Natural Nothing [] $
            unsafeCompileViewCode $ WithParam @FA2.BalanceRequestItem $ do
              getField #briOwner; dip (toField #briTokenId); pair
              stGet #sLedger
              fromOption 0
      ]
  }

allTokensView :: forall store. DaoView store
allTokensView MetadataSettings{} = View
  { vName = "all_tokens"
  , vDescription = Just
      "Get all supported tokens according to TZIP-012."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @[FA2.TokenId] Nothing [] $
            unsafeCompileViewCode $ WithoutParam $ do
              stGetField #sUnfrozenTokenId
              dip $ do stToField #sFrozenTokenId; dip nil; cons
              cons
      ]
  }

isOperatorView :: forall store. DaoView store
isOperatorView MetadataSettings{} = View
  { vName = "is_operator"
  , vDescription = Just
      "Checks whether given address is allowed to transfer given tokens \
      \that belong to given owner - according to TZIP-012."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @Bool Nothing [] $
            unsafeCompileViewCode $ WithParam @FA2.OperatorParam $ do
              dip dup
              convertOperatorParam
              pair
              stMem #sOperators
      ]
  }

tokenMetadataView :: forall store. DaoView store
tokenMetadataView MetadataSettings{ msConfig = MetadataConfig{..} } = View
  { vName = "token_metadata"
  , vDescription = Just
      "Returns metadata for given token according to TZIP-012."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @(FA2.TokenId, FA2.TokenMetadata) Nothing [] $
            unsafeCompileViewCode $ WithParam @FA2.TokenId $ do
              dip $ do
                dup
                dip emptyMap
                stToField #sFrozenTokenId
                push (Just mcFrozenTokenMetadata)
                swap
                update
                swap
                stToField #sUnfrozenTokenId
                push (Just mcUnfrozenTokenMetadata)
                swap
                update
              dup
              dip $ do
                get
                ifSome nop $ failCustom_ #fA2_TOKEN_UNDEFINED
              pair
      ]
  }

getTotalSupplyView :: forall store. DaoView store
getTotalSupplyView MetadataSettings{ msConfig = MetadataConfig{} } = View
  { vName = "get_total_supply"
  , vDescription = Just
      "Return the total number of tokens for the given token-id if known or fail if not."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @store @Natural Nothing [] $
            unsafeCompileViewCode $ WithParam @FA2.TokenId $ do
              stGet #sTotalSupply
              ifSome nop $ failCustom_ #fA2_TOKEN_UNDEFINED
      ]
  }

-- BaseDAO-specific
------------------------------------------------------------------------

permitsCounterView :: forall store. DaoView store
permitsCounterView MetadataSettings{} = View
  { vName = "GetCounter"
  , vDescription = Just
      "Returns the next counter value with which a permit should be created."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkSimpleMichelsonStorageView @store $
            unsafeCompileViewCode $ WithoutParam $
              stToField #sPermitsCounter
      ]
  }
