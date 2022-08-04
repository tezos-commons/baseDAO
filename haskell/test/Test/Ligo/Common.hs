module Test.Ligo.Common
  ( IsProposalArgument(..)
  , VariantExtraHasField(..)
  , TestableVariant(..)
  , withOriginated
  , toPeriod
  ) where

import Prelude

import Lorentz as L hiding (Contract, assert, div)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed hiding (TAddress)
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

class IsProposalArgument (variant :: Variants) a where
  toMetadata :: a -> ProposalMetadata

type VariantConstraints variant =
  ( HasBaseDAOEp (Parameter' (VariantToParam variant))
  , CEConatraints variant
  , IsoValue (VariantToExtra variant)
  , HasNoOp (ToT (VariantToExtra variant)))

class VariantExtraHasField variant name a where
  setVariantExtra :: a -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant)
  getVariantExtra :: (AsRPC (VariantToExtra variant)) -> a

class VariantConstraints variant =>
  TestableVariant variant where
  getInitialStorage :: Address -> StorageSkeleton (VariantToExtra variant)
  getContract :: Contract (ToT (Parameter' (VariantToParam variant))) (ToT (StorageSkeleton (VariantToExtra variant)))
  toProposalMetadata :: IsProposalArgument variant a => a -> ProposalMetadata
  toProposalMetadata = toMetadata @variant
  getVariantStorageRPC :: forall caps m p vd. MonadCleveland caps m => TAddress p vd -> m (StorageSkeletonRPC (VariantToExtra variant))

withOriginated
  :: forall variant caps m a. (VariantConstraints variant, TestableVariant variant, MonadCleveland caps m)
  => Integer
  -> ([Address] -> StorageSkeleton (VariantToExtra variant) -> StorageSkeleton (VariantToExtra variant))
  -> ([Address] -> StorageSkeleton (VariantToExtra variant) -> TAddress (Parameter' (VariantToParam variant)) () -> TAddress FA2.Parameter () -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  mapM (\x -> newAddress $ fromString ("address" <> (show x))) [1 ..addrCount] >>= \case
    [] -> error "Should ask for at least admin address"
    addresses@(admin: _) -> do
      dodTokenContract <- chAddress <$> originateSimple "token_contract" [] dummyFA2Contract
      let storageInitial = storageFn addresses $ getInitialStorage @variant admin
      now_level <- getLevel
      let storage = storageInitial
            { sGovernanceToken = GovernanceToken
                  { gtAddress = dodTokenContract
                  , gtTokenId = FA2.theTokenId
                  }
              , sStartLevel = now_level
            }

      baseDao <- originateUntyped $ UntypedOriginateData
        { uodName = "BaseDAO - RegistryDAO Test Contract"
        , uodBalance = zeroMutez
        , uodStorage = untypeValue $ toVal storage
        , uodContract = convertContract (getContract @variant)
        }
      tests addresses storage (TAddress baseDao) (TAddress dodTokenContract)

toPeriod :: StorageSkeleton v -> Natural
toPeriod = unPeriod . cPeriod . sConfig
