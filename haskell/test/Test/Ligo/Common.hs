-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
module Test.Ligo.Common
  ( IsProposalArgument(..)
  , VariantExtraHasField(..)
  , TestableVariant(..)
  , withOriginated
  , withOriginatedSetup
  , withOriginatedFA12
  , toPeriod
  , SizedList'(..)
  ) where

import Prelude

import Lorentz as L hiding (assert, div)
import Lorentz.Contracts.Spec.AbstractLedgerInterface qualified as FA12
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed hiding (S, TAddress)
import Morley.Tezos.Address
import Morley.Util.Peano
import Morley.Util.SizedList (SizedList, SizedList'(..))
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

withOriginatedFA12
  :: forall variant num num' caps m a num0. (IsoNatPeano num num', SingI num', TestableVariant variant, MonadCleveland caps m, num' ~ 'S num0, MonadFail m)
  => (SizedList num ImplicitAddressWithAlias -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant))
  -> (SizedList num ImplicitAddressWithAlias
        -> StorageSkeleton (VariantToExtra variant)
        -> ContractHandle (Parameter' (VariantToParam variant)) (StorageSkeleton (VariantToExtra variant)) ()
        -> ContractHandle FA2.Parameter [FA2.TransferParams] ()
        -> ContractHandle FA12.Parameter [FA12.TransferParams] ()
        -> m a)
  -> m a
withOriginatedFA12 storageFn tests = do
  withOriginatedSetup @variant (\_ _ -> originate "token_contract" [] dummyFA12Contract) storageFn (\addrs s tc fa2 dodFA12TokenContract ->
    tests addrs s tc fa2 dodFA12TokenContract)

withOriginatedSetup
  :: forall variant num num' caps m a num0 b. (IsoNatPeano num num', SingI num', TestableVariant variant, MonadCleveland caps m, num' ~ 'S num0, MonadFail m)
  => (SizedList num ImplicitAddressWithAlias -> StorageSkeleton (VariantToExtra variant) -> m b)
  -> (SizedList num ImplicitAddressWithAlias -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant))
  -> (SizedList num ImplicitAddressWithAlias
      -> StorageSkeleton (VariantToExtra variant)
      -> ContractHandle (Parameter' (VariantToParam variant)) (StorageSkeleton (VariantToExtra variant)) ()
      -> ContractHandle FA2.Parameter [FA2.TransferParams] ()
      -> b
      -> m a)
  -> m a
withOriginatedSetup setup storageFn tests = do
  addresses@(admin ::< _) <- refillables $ newAddresses $ enumAliases "address"

  dodTokenContract <- originate "token_contract" [] dummyFA2Contract
  let storageInitial = storageFn addresses $ getInitialStorage @variant (toImplicitAddress admin)
  setupResult <- setup addresses storageInitial
  now_level <- ifEmulation getLevel (getLevel >>= (\x -> pure $ x + 5))
  let storage = storageInitial
        { sGovernanceToken = GovernanceToken
              { gtAddress = toAddress dodTokenContract
              , gtTokenId = FA2.theTokenId
              }
          , sStartLevel = now_level
        }

  baseDao <- originate "BaseDAO - RegistryDAO Test Contract" storage (getContract @variant)
  advanceToLevel now_level
  tests addresses storage baseDao dodTokenContract setupResult

withOriginated
  :: forall variant num num' caps m a num0. (IsoNatPeano num num', SingI num', TestableVariant variant, MonadCleveland caps m, num' ~ 'S num0, MonadFail m)
  => (SizedList num ImplicitAddressWithAlias -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant))
  -> (SizedList num ImplicitAddressWithAlias
        -> StorageSkeleton (VariantToExtra variant)
        -> ContractHandle (Parameter' (VariantToParam variant)) (StorageSkeleton (VariantToExtra variant)) ()
        -> ContractHandle FA2.Parameter [FA2.TransferParams] ()
        -> m a)
  -> m a
withOriginated storageFn tests = withOriginatedSetup @variant (\_ _ -> pass) storageFn (const ... tests)

toPeriod :: StorageSkeleton v -> Natural
toPeriod = unPeriod . cPeriod . sConfig
