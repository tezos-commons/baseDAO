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

import Data.Aeson.TH (deriveJSON)
import Data.Version (showVersion)

import Lorentz hiding (View)
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Metadata
import Morley.Metadata.Util.Aeson (aesonOptions)

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import qualified Paths_baseDAO_ligo_meta as Paths

-- | Piece of metadata defined by user.
data MetadataConfig = MetadataConfig
  { mcName :: Text
  , mcSymbol :: Text
  , mcDecimals :: Integer
  , mcThumbnailUri :: Maybe Text
  } deriving stock (Eq, Show)

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
    { mcName = "BaseDAO Frozen Token"
    , mcSymbol = "frozen_token"
    , mcDecimals = 0
    , mcThumbnailUri = Nothing
    }


-- | Construct MetadataSetting for the contract.
mkMetadataSettings :: MetadataConfig -> MetadataSettings
mkMetadataSettings msConfig = MetadataSettings
  { msConfig
  }

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
knownBaseDAOMetadata :: MetadataSettings -> Metadata (ToT Storage)
knownBaseDAOMetadata settings@MetadataSettings{..} = mconcat
  [ name "Base DAO"
  , description "A generic smart contract on Tezos that enables a community to collectively govern resources, registries, or rules."
  , version . fromString $ showVersion Paths.version
  , license $ License "MIT" Nothing
  , authors [Author "Serokell", Author "Tocqueville Group"]
  , homepage "https://github.com/tezos-commons/baseDAO"
  , interfaces
      [ tzip 16
      , tzip 21 -- 1728fcfe0ac90463ef15e6a994b6d6a15357e373
      ]
  , views (baseDAOViews settings)
  , keyValue @[MetadataConfig] "assets"
      [ msConfig
      ]
  , errors tzipErrorList
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

----------------------------------------------------------------------------
-- JSON serializers/deserializers
----------------------------------------------------------------------------

deriveJSON aesonOptions ''MetadataConfig
