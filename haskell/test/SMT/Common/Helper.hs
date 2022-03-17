-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.Common.Helper
  ( handleTransfer
  ) where

import Universum hiding (drop, swap)

import Lorentz hiding (now, (>>))

import Ligo.BaseDAO.Common.Types
import SMT.Model.BaseDAO.Types

handleTransfer :: [SimpleOperation] -> TransferType -> [SimpleOperation]
handleTransfer ops transferType =
  case transferType of
    Token_transfer_type tt ->
      ( FA2TransferOperation
          (tt & ttContractAddress)
          (tt & ttTransferList) (toMutez 0)
      ) : ops
    Xtz_transfer_type xt ->
      ( OtherOperation (xt & xtRecipient) (show ()) (xt & xtAmount)
      ) : ops
