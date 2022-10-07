-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
module Test.Ligo.Common
  ( IsProposalArgument(..)
  , VariantExtraHasField(..)
  , TestableVariant(..)
  , withOriginated
  , toPeriod
  ) where

import Prelude

import Lorentz as L hiding (assert, div)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed hiding (TAddress)
import Morley.Tezos.Address
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

class IsProposalArgument (variant :: Variants) a where
  toMetadata :: a -> ProposalMetadata

type VariantConstraints variant =
  ( HasBaseDAOEp (Parameter' (VariantToParam variant))
  , CEConstraints variant
  , IsoValue (VariantToExtra variant)
  , HasNoOp (ToT (VariantToExtra variant)))

class VariantExtraHasField variant name a where
  setVariantExtra :: a -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant)
  getVariantExtra :: (AsRPC (VariantToExtra variant)) -> a

class VariantConstraints variant =>
  TestableVariant variant where
  getInitialStorage :: ImplicitAddress -> StorageSkeleton (VariantToExtra variant)
  getContract :: L.Contract (Parameter' (VariantToParam variant)) (StorageSkeleton (VariantToExtra variant)) ()
  toProposalMetadata :: IsProposalArgument variant a => a -> ProposalMetadata
  toProposalMetadata = toMetadata @variant
  getVariantStorageRPC :: forall caps m . MonadCleveland caps m => ContractAddress -> m (StorageSkeletonRPC (VariantToExtra variant))

withOriginated
  :: forall variant caps m a. (VariantConstraints variant, TestableVariant variant, MonadCleveland caps m)
  => Integer
  -> ([ImplicitAddress] -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant))
  -> ([ImplicitAddress] -> StorageSkeleton (VariantToExtra variant) -> ContractHandle (Parameter' (VariantToParam variant)) (StorageSkeleton (VariantToExtra variant)) () -> ContractHandle FA2.Parameter [FA2.TransferParams] () -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  mapM (\x -> refillable $ newAddress $ fromString ("address" <> (show x))) [1 ..addrCount] >>= \case
    [] -> error "Should ask for at least admin address"
    addresses@(admin: _) -> do
      dodTokenContract <- originate "token_contract" [] dummyFA2Contract
      let storageInitial = storageFn addresses $ getInitialStorage @variant admin
      now_level <- ifEmulation getLevel (getLevel >>= (\x -> pure $ x + 5))
      let storage = storageInitial
            { sGovernanceToken = GovernanceToken
                  { gtAddress = MkAddress $ chAddress dodTokenContract
                  , gtTokenId = FA2.theTokenId
                  }
              , sStartLevel = now_level
            }

      baseDao <- originate "BaseDAO - RegistryDAO Test Contract" storage (getContract @variant)
      advanceToLevel now_level
      tests addresses storage baseDao dodTokenContract

toPeriod :: StorageSkeleton v -> Natural
toPeriod = unPeriod . cPeriod . sConfig
