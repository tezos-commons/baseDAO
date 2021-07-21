-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.Common.Helper
  ( findBigMap
  , handleTransfer
  , unpackWithError
  ) where

import Universum hiding (drop, swap)

import qualified Data.Map as Map

import Lorentz hiding (now, (>>))
import qualified Michelson.Typed as T

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types


findBigMap :: MText -> ContractExtra -> ByteString
findBigMap field extras =
  Map.lookup field (T.unBigMap $ unDynamic extras)
    & fromMaybe (error "MISSING_VALUE")

unpackWithError :: forall a. (NiceUnpackedValue a) => ByteString -> a
unpackWithError packed =
  lUnpackValueRaw @a packed
    & fromRight (error "UNPACKING_FAILED")

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