-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ligo.LambdaTreasuryDAO
  ( test_LambdaTreasuryDAO
  ) where

import Prelude

import Data.Map qualified as Map
import Data.Set qualified as S
import Test.Tasty (TestTree)

import Lorentz as L hiding (assert, div)
import Morley.Tezos.Address
import Test.Cleveland

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.LambdaDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.Common
import Test.Ligo.TreasuryDAO
import Test.Ligo.TreasuryDAO.Types

instance TestableVariant 'LambdaTreasury where
  getInitialStorage admin = initialStorageWithExplictLambdaDAOConfig admin
  getContract = baseDAOLambdaTreasuryLigo
  getVariantStorageRPC addr = getStorage @(StorageSkeleton (VariantToExtra 'LambdaTreasury)) addr

instance IsProposalArgument 'LambdaTreasury TransferProposal where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "transfer_proposal" $ lPackValueRaw a

instance IsProposalArgument 'LambdaTreasury Address where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "update_guardian_proposal" $ lPackValueRaw a

instance IsProposalArgument 'LambdaTreasury (Maybe KeyHash) where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "update_contract_delegate_proposal" $ lPackValueRaw a

instance VariantExtraHasField 'LambdaTreasury "ProposalReceivers" (Set Address) where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|proposal_receivers|] a)
  getVariantExtra = getInHandlerStorage [mt|proposal_receivers|]

instance VariantExtraHasField 'LambdaTreasury "MinXtzAmount" Mutez where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|min_xtz_amount|] a)
  getVariantExtra = getInHandlerStorage [mt|min_xtz_amount|]

instance VariantExtraHasField 'LambdaTreasury "FrozenExtraValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|frozen_extra_value|] a)
  getVariantExtra = getInHandlerStorage [mt|frozen_extra_value|]

instance VariantExtraHasField 'LambdaTreasury "FrozenScaleValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|frozen_scale_value|] a)
  getVariantExtra = getInHandlerStorage [mt|frozen_scale_value|]

instance VariantExtraHasField 'LambdaTreasury "SlashScaleValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|slash_scale_value|] a)
  getVariantExtra = getInHandlerStorage [mt|slash_scale_value|]

instance VariantExtraHasField 'LambdaTreasury "SlashDivisionValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|slash_division_value|] a)
  getVariantExtra = getInHandlerStorage [mt|slash_division_value|]

instance VariantExtraHasField 'LambdaTreasury "MaxProposalSize" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|max_proposal_size|] a)
  getVariantExtra = getInHandlerStorage [mt|max_proposal_size|]

-- | We test non-token entrypoints of the BaseDAO contract here
test_LambdaTreasuryDAO :: TestTree
test_LambdaTreasuryDAO = treasuryDAOTests @'LambdaTreasury

initialStorage :: ImplicitAddress -> LambdaStorage
initialStorage admin = let
  fs = baseDAOLambdatreasuryStorageLigo
  in fs { sAdmin = MkAddress admin, sConfig = (sConfig fs)
            { cPeriod = 11
            , cProposalFlushLevel = 22
            , cProposalExpiredLevel = 33
            , cGovernanceTotalSupply = 100

      }}

initialStorageWithExplictLambdaDAOConfig :: ImplicitAddress -> LambdaStorage
initialStorageWithExplictLambdaDAOConfig admin = (initialStorage admin)
  & setExtra (setInHandlerStorage [mt|proposal_receivers|] (mempty :: S.Set Address))
  & setExtra (setInHandlerStorage [mt|frozen_scale_value|] (1 :: Natural))
  & setExtra (setInHandlerStorage [mt|frozen_extra_value|] (0 :: Natural))
  & setExtra (setInHandlerStorage [mt|slash_scale_value|] (1 :: Natural))
  & setExtra (setInHandlerStorage [mt|slash_division_value|] (1 :: Natural))
  & setExtra (setInHandlerStorage [mt|min_xtz_amount|] [tz|2u|])
  & setExtra (setInHandlerStorage [mt|max_xtz_amount|] [tz|5u|])
  & setExtra (setInHandlerStorage [mt|max_proposal_size|] (1000 :: Natural))

setInHandlerStorage :: NicePackedValue v => MText -> v -> LambdaExtra -> LambdaExtra
setInHandlerStorage k v e = e { leHandlerStorage = Map.insert k (lPackValueRaw v) (leHandlerStorage e) }

getInHandlerStorage :: NiceUnpackedValue v => MText -> LambdaExtraRPC -> v
getInHandlerStorage k e = fromRight (error "Unpacking failed") $ lUnpackValueRaw $ fromMaybe (error "Key not found in Handler storage") $ Map.lookup k (leHandlerStorageRPC e)
