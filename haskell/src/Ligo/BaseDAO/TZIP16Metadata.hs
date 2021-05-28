-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-16 metadata.
module Ligo.BaseDAO.TZIP16Metadata
  ( MetadataConfig (..)
  , MetadataSettings (..)
  , defaultMetadataConfig
  , knownBaseDAOMetadata
  , mkMetadataSettings

  , baseDAOViews
  , permitsCounterView
  , governanceTokenView
  ) where

import qualified Universum as U

import Data.Version (showVersion)

import Lorentz hiding (View)
import Lorentz.Contracts.Spec.TZIP16Interface
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Metadata

import Ligo.BaseDAO.Types
import qualified Paths_baseDAO_ligo_meta as Paths

-- | Piece of metadata defined by user.
data MetadataConfig = MetadataConfig
  { mcFrozenTokenMetadata :: FA2.TokenMetadata
  }

-- | All the information for instantiating metadata.
--
-- This includes pieces defined by user (if any) like some constants for
-- off-chain views, as well as abstract storage accessors of the contract.
data MetadataSettings = MetadataSettings
  { msConfig :: MetadataConfig
  }

-- | Default values for config.
defaultMetadataConfig :: MetadataConfig
defaultMetadataConfig =
  MetadataConfig
    { mcFrozenTokenMetadata =
        FA2.mkTokenMetadata "frozen_token" "Frozen Token" "8"
    }

-- | Construct MetadataSetting for the contract.
mkMetadataSettings :: MetadataConfig -> MetadataSettings
mkMetadataSettings msConfig = MetadataSettings
  { msConfig
  }

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
knownBaseDAOMetadata :: MetadataSettings -> Metadata (ToT Storage)
knownBaseDAOMetadata settings = mconcat
  [ version . fromString $ showVersion Paths.version
  , license $ License "MIT" Nothing
  , authors [Author "Serokell", Author "Tocqueville Group"]
  , homepage "https://github.com/tqtezos/baseDAO"
  , interfaces [tzip 17]
  , views (baseDAOViews settings)
  ]

------------------------------------------------------------------------
-- Off-chain views
------------------------------------------------------------------------

baseDAOViews :: MetadataSettings -> [View $ ToT Storage]
baseDAOViews = U.sequence
  [ governanceTokenView
  , permitsCounterView
  ]

type DaoView = MetadataSettings -> View (ToT Storage)

-- BaseDAO-specific
------------------------------------------------------------------------

governanceTokenView :: DaoView
governanceTokenView MetadataSettings{} = View
  { vName = "governance_token"
  , vDescription = Just
      "Return the address and token_id of the associated FA2 contract."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @Storage Nothing [] $
            unsafeCompileViewCode $ WithoutParam $ do
              stToField #sGovernanceToken
      ]
  }

permitsCounterView :: DaoView
permitsCounterView MetadataSettings{} = View
  { vName = "GetCounter"
  , vDescription = Just
      "Returns the next counter value with which a permit should be created."
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkSimpleMichelsonStorageView @Storage $
            unsafeCompileViewCode $ WithoutParam $
              stToField #sPermitsCounter
      ]
  }