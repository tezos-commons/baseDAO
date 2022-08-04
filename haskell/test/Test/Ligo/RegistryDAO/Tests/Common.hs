
-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.Common
  ( RegistryTestConstraints
  , expectFailProposalCheck
  , xtzTransferType
  , tokenTransferType
  ) where

import Prelude

import Lorentz as L hiding (Contract, assert, div)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Test.Cleveland

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Types

type RegistryTestConstraints variant =
  ( IsProposalArgument variant TransferProposal
  , IsProposalArgument variant ConfigProposal
  , IsProposalArgument variant Address
  , IsProposalArgument variant UpdateReceiverParam
  , IsProposalArgument variant (Maybe KeyHash)
  , ParameterContainsEntrypoints (Parameter' (VariantToParam variant)) '["Lookup_registry" :> LookupRegistryParam]
  , TestableVariant variant
  , VariantExtraHasField variant "MinXtzAmount" Mutez
  , VariantExtraHasField variant "MaxProposalSize" Natural
  , VariantExtraHasField variant "FrozenExtraValue" Natural
  , VariantExtraHasField variant "FrozenScaleValue" Natural
  , VariantExtraHasField variant "SlashScaleValue" Natural
  , VariantExtraHasField variant "SlashDivisionValue" Natural
  , VariantExtraHasField variant "ProposalReceivers" (Set Address)
  )

expectFailProposalCheck
  :: (MonadCleveland caps m)
  => MText -> m a -> m ()
expectFailProposalCheck err =
  expectFailedWith (failProposalCheck, err)

--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

xtzTransferType :: Word32 -> Address -> TransferType
xtzTransferType amt toAddr = Xtz_transfer_type XtzTransfer
  { xtAmount = toMutez amt
  , xtRecipient = toAddr
  }

tokenTransferType :: Address -> Address -> Address -> TransferType
tokenTransferType contractAddr fromAddr toAddr = Token_transfer_type TokenTransfer
  { ttContractAddress = contractAddr
  , ttTransferList =
    [ FA2.TransferItem
      { tiFrom = fromAddr
      , tiTxs =
        [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = FA2.theTokenId
          , tdAmount = 10
          }
        ]
      }
    ]
  }
