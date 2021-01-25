-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Simplest DAOs on top of BaseDAO.
module Lorentz.Helper
  ( failAsText
  ) where

import Lorentz

failAsText :: Label name -> s :-> t
failAsText label =
  failUsing (errorTagToMText label)
