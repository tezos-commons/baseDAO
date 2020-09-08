-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Managed ledger which is compatible with FA1.2 standard and
-- extended with administrator functionality.

module Lorentz.Contracts.BaseDAO
    ( Parameter(..)

    , Storage
    , StorageC
    , LedgerValue
    , mkStorage
    , storageNotes

    , contractScript
    ) where

import Lorentz.Contracts.BaseDAO.Impl
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface
