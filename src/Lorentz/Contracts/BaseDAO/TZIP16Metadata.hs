-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-16 metadata.
module Lorentz.Contracts.BaseDAO.TZIP16Metadata
  ( knownBaseDAOMetadata
  ) where

import Data.Version (showVersion)

import Lorentz hiding (View)
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Metadata

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

baseDAOViews :: [View $ ToT MetadataStorage]
baseDAOViews =
  [ View
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
  ]
