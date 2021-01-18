-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | TZIP-16 metadata.
module Ligo.BaseDAO.TZIP16Metadata
  ( mkMetadataSettingsL
  ) where

import Ligo.BaseDAO.Types
import Lorentz.Contracts.BaseDAO.TZIP16Metadata

-- | Construct settings for LIGO version of the contract.
mkMetadataSettingsL :: MetadataConfig -> MetadataSettings StorageL
mkMetadataSettingsL msConfig = MetadataSettings
  { msConfig
  , mcOperatorsL = sOperatorsLens
  }
