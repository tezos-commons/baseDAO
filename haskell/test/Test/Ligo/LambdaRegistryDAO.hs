-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.LambdaRegistryDAO
  ( test_LambdaRegistryDAO
  ) where

import Prelude

import Data.Map qualified as Map
import Data.Set qualified as S
import Test.Tasty (TestTree)

import Lorentz as L hiding (assert, div)
import Test.Cleveland

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.LambdaDAO.Types
import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.Common
import Test.Ligo.RegistryDAO
import Test.Ligo.RegistryDAO.Types

getStorageRPCLambda :: forall p caps vd m. MonadCleveland caps m => TAddress p vd ->  m (StorageSkeletonRPC (VariantToExtra 'Lambda))
getStorageRPCLambda addr = getStorage @(StorageSkeleton (VariantToExtra 'Lambda)) (unTAddress addr)

instance TestableVariant 'LambdaRegistry where
  getInitialStorage admin = initialStorageWithExplictLambdaDAOConfig admin
  getContract = baseDAOLambdaRegistryLigo
  getVariantStorageRPC addr = getStorage @(StorageSkeleton (VariantToExtra 'LambdaRegistry)) (unTAddress addr)

instance IsProposalArgument 'LambdaRegistry TransferProposal where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "transfer_proposal" $ lPackValueRaw a

instance IsProposalArgument 'LambdaRegistry ConfigProposal where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "configuration_proposal" $ lPackValueRaw a

instance IsProposalArgument 'LambdaRegistry Address where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "update_guardian_proposal" $ lPackValueRaw a

instance IsProposalArgument 'LambdaRegistry UpdateReceiverParam where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "update_receivers_proposal" $ lPackValueRaw a

instance IsProposalArgument 'LambdaRegistry (Maybe KeyHash) where
  toMetadata a = lPackValueRaw @LambdaDaoProposalMetadata $ Execute_handler $ ExecuteHandlerParam "update_contract_delegate_proposal" $ lPackValueRaw a

instance VariantExtraHasField 'LambdaRegistry "ProposalReceivers" (Set Address) where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|proposal_receivers|] a)
  getVariantExtra = getInHandlerStorage [mt|proposal_receivers|]

instance VariantExtraHasField 'LambdaRegistry "MinXtzAmount" Mutez where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|min_xtz_amount|] a)
  getVariantExtra = getInHandlerStorage [mt|min_xtz_amount|]

instance VariantExtraHasField 'LambdaRegistry "FrozenExtraValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|frozen_extra_value|] a)
  getVariantExtra = getInHandlerStorage [mt|frozen_extra_value|]

instance VariantExtraHasField 'LambdaRegistry "FrozenScaleValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|frozen_scale_value|] a)
  getVariantExtra = getInHandlerStorage [mt|frozen_scale_value|]

instance VariantExtraHasField 'LambdaRegistry "SlashScaleValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|slash_scale_value|] a)
  getVariantExtra = getInHandlerStorage [mt|slash_scale_value|]

instance VariantExtraHasField 'LambdaRegistry "SlashDivisionValue" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|slash_division_value|] a)
  getVariantExtra = getInHandlerStorage [mt|slash_division_value|]

instance VariantExtraHasField 'LambdaRegistry "MaxProposalSize" Natural where
  setVariantExtra a = setExtra (setInHandlerStorage [mt|max_proposal_size|] a)
  getVariantExtra = getInHandlerStorage [mt|max_proposal_size|]

-- | We test non-token entrypoints of the BaseDAO contract here
test_LambdaRegistryDAO :: [TestTree]
test_LambdaRegistryDAO = registryDAOTests @'LambdaRegistry

initialStorage :: Address -> LambdaStorage
initialStorage admin = let
  fs = baseDAOLambdaregistryStorageLigo
  in fs { sAdmin = admin, sConfig = (sConfig fs)
            { cPeriod = 20
            , cProposalFlushLevel = 40
            , cProposalExpiredLevel = 100
            , cGovernanceTotalSupply = 100

      }}

initialStorageWithExplictLambdaDAOConfig :: Address -> LambdaStorage
initialStorageWithExplictLambdaDAOConfig admin = (initialStorage admin)
  & setExtra (setInHandlerStorage [mt|registry|] (mempty :: Map RegistryKey RegistryValue))
  & setExtra (setInHandlerStorage [mt|registry_affected|] (mempty :: Map RegistryKey ProposalKey))
  & setExtra (setInHandlerStorage [mt|proposal_receivers|] (mempty :: S.Set Address))
  & setExtra (setInHandlerStorage [mt|frozen_scale_value|] (1 :: Natural))
  & setExtra (setInHandlerStorage [mt|frozen_extra_value|] (0 :: Natural))
  & setExtra (setInHandlerStorage [mt|slash_scale_value|] (1 :: Natural))
  & setExtra (setInHandlerStorage [mt|slash_division_value|] (1 :: Natural))
  & setExtra (setInHandlerStorage [mt|min_xtz_amount|] [tz|2u|])
  & setExtra (setInHandlerStorage [mt|max_xtz_amount|] [tz|5u|])
  & setExtra (setInHandlerStorage [mt|max_proposal_size|] (100 :: Natural))

setInHandlerStorage :: NicePackedValue v => MText -> v -> LambdaExtra -> LambdaExtra
setInHandlerStorage k v e = e { leHandlerStorage = Map.insert k (lPackValueRaw v) (leHandlerStorage e) }

getInHandlerStorage :: NiceUnpackedValue v => MText -> LambdaExtraRPC -> v
getInHandlerStorage k e = fromRight (error "Unpacking failed") $ lUnpackValueRaw $ fromMaybe (error "Key not found in Handler storage") $ Map.lookup k (leHandlerStorageRPC e)
