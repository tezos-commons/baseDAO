-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Managed ledger which is compatible with FA1.2 standard and
-- extended with administrator functionality.

module Lorentz.Contracts.ManagedLedger
    ( Parameter(..)

    , Storage
    , StorageC
    , LedgerValue
    , mkStorage
    , storageNotes

    , managedLedgerContract
    ) where

import Lorentz.Contracts.ManagedLedger.Impl
import Lorentz.Contracts.ManagedLedger.Types
import Lorentz.Contracts.Spec.ManagedLedgerInterface
