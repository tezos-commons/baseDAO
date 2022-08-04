-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO
  ( RegistryTestConstraints
  , test_RegistryDAO
  , registryDAOTests
  ) where

import Prelude

import Data.Set qualified as S
import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (Contract, assert, div)
import Test.Cleveland

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Tests.EmptyProposal
import Test.Ligo.RegistryDAO.Tests.ExecuteWonProposal (executeWonProposal)
import Test.Ligo.RegistryDAO.Tests.FlushRegistryUpdates
import Test.Ligo.RegistryDAO.Tests.FlushTransferProposal
import Test.Ligo.RegistryDAO.Tests.LargeProposal
import Test.Ligo.RegistryDAO.Tests.NotEnoughFrozen
import Test.Ligo.RegistryDAO.Tests.ProposeXtzProposal
import Test.Ligo.RegistryDAO.Tests.RegistryView
import Test.Ligo.RegistryDAO.Tests.RequiredFrozen
import Test.Ligo.RegistryDAO.Tests.TokensToBurn
import Test.Ligo.RegistryDAO.Tests.UpdateDelegate
import Test.Ligo.RegistryDAO.Tests.UpdateGuardian
import Test.Ligo.RegistryDAO.Tests.UpdateReceivers
import Test.Ligo.RegistryDAO.Tests.UpdateReceiversDelete
import Test.Ligo.RegistryDAO.Tests.ZeroXtz
import Test.Ligo.RegistryDAO.Types

instance IsProposalArgument 'Registry TransferProposal where
  toMetadata a = lPackValueRaw @RegistryDaoProposalMetadata $ Transfer_proposal a

instance IsProposalArgument 'Registry ConfigProposal where
  toMetadata a = lPackValueRaw @RegistryDaoProposalMetadata $ Configuration_proposal a

instance IsProposalArgument 'Registry Address where
  toMetadata a = lPackValueRaw @RegistryDaoProposalMetadata $ Update_guardian a

instance IsProposalArgument 'Registry (Maybe KeyHash) where
  toMetadata a = lPackValueRaw @RegistryDaoProposalMetadata $ Update_contract_delegate a

instance IsProposalArgument 'Registry UpdateReceiverParam where
  toMetadata a = lPackValueRaw @RegistryDaoProposalMetadata $ Update_receivers_proposal a

instance VariantExtraHasField 'Registry "MinXtzAmount" Mutez where
  setVariantExtra v = setExtra (\re -> re { reMinXtzAmount = v })
  getVariantExtra = reMinXtzAmountRPC

instance VariantExtraHasField 'Registry "ProposalReceivers" (Set Address) where
  setVariantExtra v = setExtra (\re -> re { reProposalReceivers = v })
  getVariantExtra = reProposalReceiversRPC

instance VariantExtraHasField 'Registry "MaxProposalSize" Natural where
  setVariantExtra v = setExtra (\re -> re { reMaxProposalSize = v })
  getVariantExtra = reMaxProposalSizeRPC

instance VariantExtraHasField 'Registry "FrozenExtraValue" Natural where
  setVariantExtra v = setExtra (\re -> re { reFrozenExtraValue = v })
  getVariantExtra = reFrozenExtraValueRPC

instance VariantExtraHasField 'Registry "FrozenScaleValue" Natural where
  setVariantExtra v = setExtra (\re -> re { reFrozenScaleValue = v })
  getVariantExtra = reFrozenScaleValueRPC

instance VariantExtraHasField 'Registry "SlashScaleValue" Natural where
  setVariantExtra v = setExtra (\re -> re { reSlashScaleValue = v })
  getVariantExtra = reSlashScaleValueRPC

instance VariantExtraHasField 'Registry "SlashDivisionValue" Natural where
  setVariantExtra v = setExtra (\re -> re { reSlashDivisionValue = v })
  getVariantExtra = reSlashDivisionValueRPC

instance TestableVariant 'Registry where
  getInitialStorage admin = initialStorageWithExplictRegistryDAOConfig admin
  getContract = baseDAORegistryLigo
  getVariantStorageRPC addr = getStorage @(StorageSkeleton (VariantToExtra 'Registry)) (unTAddress addr)

getStorageRPCRegistry :: forall p caps vd m. MonadCleveland caps m => TAddress p vd ->  m (StorageSkeletonRPC (VariantToExtra 'Registry))
getStorageRPCRegistry addr = getStorage @(StorageSkeleton (VariantToExtra 'Registry)) (unTAddress addr)

test_RegistryDAO :: [TestTree]
test_RegistryDAO = registryDAOTests @'Registry

-- | We test non-token entrypoints of the BaseDAO contract here
-- test_RegistryDAO :: forall (variant :: Variants). (IsoValue (VariantToExtra variant), HasNoOp (ToT (VariantToExtra variant))) => [TestTree]
registryDAOTests
  :: forall variant. RegistryTestConstraints variant => [TestTree]
registryDAOTests =
  [ testGroup "RegistryDAO Tests"
    [ emptyProposalTest @variant
    , largeProposalTest @variant
    , zeroXtzTransfer @variant
    , notEnoughFrozen @variant
    , requiredFrozen @variant
    , tokensToBurn @variant
    , executeWonProposal @variant
    , registryView @variant
    , flushTransferProposal @variant
    , proposeXtzProposal @variant
    , flushRegistryUpdates @variant
    , updateReceivers @variant
    , updateGuardian @variant
    , updateDelegate @variant
    , updateReceiversDelete @variant
    ]
  ]

-- Here we parse the storage value from compiled ligo storage, which
-- contains the RegistryDAO callbacks implemented in LIGO, and we just use
-- `fromVal` to convert it to a 'Storage'. Then we can set the
-- RegistryDAO configuration values using the setExtra function below, and
-- initialize the contract using it. This let us have the callbacks from LIGO
-- in storage, and allows to tweak RegistryDAO configuration in tests.
initialStorage :: Address -> RegistryStorage
initialStorage admin = let
  fs = baseDAORegistryStorageLigo { sExtra = def }
  in fs { sAdmin = admin, sConfig = (sConfig fs)
            { cPeriod = 11
            , cProposalFlushLevel = 22
            , cProposalExpiredLevel = 33
            , cGovernanceTotalSupply = 100

      }}

initialStorageWithExplictRegistryDAOConfig :: Address -> RegistryStorage
initialStorageWithExplictRegistryDAOConfig admin = (initialStorage admin)
  & setExtra (\re -> re { reRegistry = mempty })
  & setExtra (\re -> re { reRegistryAffected = mempty })
  & setExtra (\re -> re { reProposalReceivers = S.empty })
  & setExtra (\re -> re { reFrozenScaleValue = 1 })
  & setExtra (\re -> re { reFrozenExtraValue = 0 })
  & setExtra (\re -> re { reSlashScaleValue = 1 })
  & setExtra (\re -> re { reSlashDivisionValue = 1 })
  & setExtra (\re -> re { reMinXtzAmount = 2 })
  & setExtra (\re -> re { reMaxXtzAmount = 5 })
  & setExtra (\re -> re { reMaxProposalSize = 100 })

