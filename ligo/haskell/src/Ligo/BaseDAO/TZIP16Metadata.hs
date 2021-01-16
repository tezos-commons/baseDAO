-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-16 metadata.
module Ligo.BaseDAO.TZIP16Metadata
  ( defaultMetadataConfigL
  ) where

import Ligo.BaseDAO.Types
import Lorentz.Contracts.BaseDAO.TZIP16Metadata

-- | Default values for config.
-- Should we in fact fill it from CLI?
defaultMetadataConfigL :: MetadataConfig StorageL
defaultMetadataConfigL =
  let ConfigL{..} = defaultConfigL
  in MetadataConfig
    { mcFrozenTokenMetadata = cFrozenTokenMetadata
    , mcUnfrozenTokenMetadata = cUnfrozenTokenMetadata

    , mcOperatorsL = sOperatorsLens
    }
