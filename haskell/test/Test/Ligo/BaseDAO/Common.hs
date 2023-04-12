-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ligo.BaseDAO.Common
  ( ContractType(..)
  , DaoOriginateData(..)
  , OriginateFn

  , dummyFA2Contract
  , dummyFA12Contract
  , dummyGuardianContract
  , makeProposalKey
  , addDataToSign
  , permitProtect
  , sendXtz
  , sendXtzWithAmount

  , checkStorage
  , createSampleProposal
  , createSampleProposals
  , defaultQuorumThreshold
  , originateLigoDaoWithConfigDesc
  , originateLigoDao

  -- * Helpers
  , metadataSize
  , refillables

  -- * Re-export
  , module Errors
  , module StorageHelper
  ) where

import Universum hiding (drop, swap)

import Data.ByteString qualified as BS
import Lorentz hiding (assert, now, (>>))
import Lorentz.Contracts.Spec.AbstractLedgerInterface qualified as FA12
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed.Scope (HasNoOp)
import Morley.Util.SizedList (SizedList'(..), pattern (::<))
import Test.Cleveland

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common.Errors as Errors
import Test.Ligo.BaseDAO.Common.StorageHelper as StorageHelper
import Test.Ligo.BaseDAO.Proposal.Config (ConfigDesc, fillConfig)

type OriginateFn a m = QuorumThreshold -> m (DaoOriginateData a)

instance HasRPCRepr FA2.TransferItem where
  type AsRPC FA2.TransferItem = FA2.TransferItem

data ContractType
  = BaseDaoContract
  | RegistryDaoContract
  | TreasuryDaoContract
  deriving stock Eq

data DaoOriginateData a = DaoOriginateData
  { dodOwner1 :: ImplicitAddressWithAlias
  , dodOperator1 :: ImplicitAddressWithAlias
  , dodOwner2 :: ImplicitAddressWithAlias
  , dodOperator2 :: ImplicitAddressWithAlias
  , dodDao :: ContractHandle (Parameter' (VariantToParam a)) (StorageSkeleton (VariantToExtra a)) ()
  , dodTokenContract :: ContractHandle FA2.Parameter [FA2.TransferParams] ()
  , dodAdmin :: ImplicitAddressWithAlias
  , dodGuardian :: ContractHandle (Address, ProposalKey) () ()
  , dodPeriod :: Natural
  }

-- | A dummy contract with FA2 parameter that remembers the
-- transfer calls.
dummyFA2Contract :: Contract FA2.Parameter [FA2.TransferParams] ()
dummyFA2Contract = defaultContract $
  unpair #
    (entryCase @FA2.Parameter (Proxy @PlainEntrypointsKind)
      ( #cBalance_of /-> Lorentz.drop
      , #cTransfer /-> cons
      , #cUpdate_operators /-> Lorentz.drop
      )) # nil # pair

-- | A dummy contract with FA1.2 parameter that remembers the
-- transfer calls.
dummyFA12Contract :: Contract FA12.Parameter [FA12.TransferParams] ()
dummyFA12Contract = defaultContract $
  unpair #
    (entryCase @FA12.Parameter (Proxy @PlainEntrypointsKind)
      ( #cTransfer /-> cons
      , #cGetTotalSupply /-> Lorentz.drop
      , #cGetBalance /-> Lorentz.drop
      )) # nil # pair


-- | A dummy contract that act as guardian contract for BaseDAO
dummyGuardianContract :: Contract (Address, ProposalKey) () ()
dummyGuardianContract = defaultContract $
  unpair # unpair #
  stackType @'[Address, ProposalKey, ()] #
  contractCalling @Parameter (Call @"Drop_proposal") #
  ifSome (
      swap #
      push zeroMutez # swap #
      transferTokens # nil # swap # cons
    ) (drop # nil) #
  stackType @'[[Operation], ()] #
  pair

makeProposalKey :: ProposeParams -> ProposalKey
makeProposalKey params = toHashHs $ lPackValue params

addDataToSign
  :: (MonadCleveland caps m)
  => ContractHandle p s ()
  -> Nonce
  -> d
  -> m (DataToSign d, d)
addDataToSign (toAddress -> dsContract) dsNonce dsData = do
  dsChainId <- getChainId
  return (DataToSign{..}, dsData)

-- | Add a permit from given user.
permitProtect
  :: (MonadCleveland caps m, NicePackedValue a)
  => ImplicitAddressWithAlias -> (DataToSign a, a) -> m (PermitProtected a)
permitProtect author (toSign, a) = do
  pKey <- getPublicKey author
  pSignature <- signBinary (lPackValue toSign) author
  return PermitProtected
    { ppArgument = a
    , ppPermit = Just Permit{..}
    }

sendXtz
  :: (MonadCleveland caps m, HasCallStack)
  => ContractHandle cp st vd  -> m ()
sendXtz addr = withFrozenCallStack $
  transfer (chAddress addr) [tz|0.5|]

sendXtzWithAmount
  :: (MonadCleveland caps m, HasCallStack)
  => Mutez -> ContractHandle cp st vd -> m ()
sendXtzWithAmount amt addr = withFrozenCallStack $
  transfer (chAddress addr) amt

defaultQuorumThreshold :: QuorumThreshold
defaultQuorumThreshold = mkQuorumThreshold 1 100

type OriginationCEConstraints cep =
  ( Typeable cep
  , Default (VariantToExtra cep)
  , NiceStorage (StorageSkeleton (VariantToExtra cep))
  , IsoValue (VariantToExtra cep)
  , NiceParameterFull (VariantToParam cep)
  , HasNoOp (ToT (VariantToExtra cep)))

originateLigoDaoWithConfig
 :: forall cep caps m. (OriginationCEConstraints cep, MonadCleveland caps m)
 => (VariantToExtra cep)
 -> Config
 -> OriginateFn cep m
originateLigoDaoWithConfig extra config qt =
  refillables (newAddresses $ "owner1" :< "operator1" :< "owner2" :< "operator2" :< "admin" :< Nil) >>=
    \(owner1 ::< operator1 ::< owner2 ::< operator2 ::< admin ::< Nil') -> do
      owner1Balance <- getBalance owner1

      when (owner1Balance < 2_e6) $ do
        let fundAddress x = transfer x [tz|5|]
        mapM_ fundAddress [owner1, operator1, owner2, operator2, admin]

      tokenContract <- originate "TokenContract" [] dummyFA2Contract
      guardianContract <- originate "guardian" () dummyGuardianContract
      currentLevel <- getLevel

      let originationOffset = 12
      let storage =
                ( mkStorage
                  ! #extra extra
                  ! #admin (toImplicitAddress admin)
                  ! #metadata mempty
                  ! #tokenAddress (toAddress tokenContract)
                  ! #level (currentLevel + originationOffset)
                    -- We add some levels to offset for the delay caused by the origination function
                    -- So the expectation is that by the time origination has finished, we will be closer
                    -- to the actual block that includes origination so that the tests have a lesser chance
                    -- to fail due to this difference.
                  ! #quorumThreshold qt
                  ! #config config
                )
                { sGuardian = toAddress guardianContract
                }

      dao <- originate "BaseDAO" storage (getBaseDAOContract @cep)
      advanceToLevel (currentLevel + originationOffset)

      pure $ DaoOriginateData owner1 operator1 owner2 operator2 dao tokenContract
          admin guardianContract (unPeriod $ cPeriod config)

originateLigoDaoWithConfigDesc
 :: forall cep caps m. (OriginationCEConstraints cep, MonadCleveland caps m)
 => VariantToExtra cep
 -> ConfigDesc Config
 -> OriginateFn cep m
originateLigoDaoWithConfigDesc extra config =
  originateLigoDaoWithConfig @cep extra (fillConfig config defaultConfig)

originateLigoDao :: forall cep caps m. (OriginationCEConstraints cep, MonadCleveland caps m) => OriginateFn cep m
originateLigoDao =
  originateLigoDaoWithConfig @cep def defaultConfig

createSampleProposal
  :: (MonadCleveland caps m, HasCallStack)
  => Int -> ImplicitAddressWithAlias -> ContractHandle Parameter Storage vd -> m ProposalKey
createSampleProposal counter dodOwner dao = do
  let (pk, action) = createSampleProposal_ counter (toImplicitAddress dodOwner) dao
  withSender dodOwner action
  pure pk

createSampleProposal_
  :: (MonadOps m, HasCallStack)
  => Int -> ImplicitAddress -> ContractHandle Parameter Storage vd -> (ProposalKey, m ())
createSampleProposal_ counter dodOwner1 dao =
  let params = ProposeParams
        { ppFrom = toAddress dodOwner1
        , ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer $ fromIntegral counter
        }
  in (makeProposalKey params, transfer dao $ calling (ep @"Propose") params)

-- TODO consider making this polymorphic on the input/output size
createSampleProposals
  :: (MonadCleveland caps m, HasCallStack)
  => (Int, Int)
  -> ImplicitAddressWithAlias
  -> ContractHandle Parameter Storage vd
  -> m (ProposalKey, ProposalKey)
createSampleProposals (counter1, counter2) dodOwner1 dao = do
  let (pk1, action1) = createSampleProposal_ counter1 (toImplicitAddress dodOwner1) dao
  let (pk2, action2) = createSampleProposal_ counter2 (toImplicitAddress dodOwner1) dao
  withSender dodOwner1 . inBatch $ do
    action1
    action2
    return ()
  pure (pk1, pk2)

checkStorage
  :: forall st caps m p.
      ( AsRPC st ~ st, MonadCleveland caps m
      , HasCallStack, Eq st, NiceStorage st)
  => ContractHandle p st () -> st -> m ()
checkStorage addr expected = do
  realSt <- getStorage addr
  assert (expected == realSt) "Unexpected storage"

metadataSize :: ByteString -> Natural
metadataSize md = fromIntegral $ BS.length md
