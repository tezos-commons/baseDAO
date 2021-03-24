-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE NumericUnderscores #-}

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Contain common functions and types that are used
-- in the shared tests.
module Ligo.BaseDAO.ShareTest.Common
  ( OriginateFn
  , IsLorentz
  , expectFailed
  , totalSupplyFromLedger

  , mkFA2View
  , checkTokenBalance
  , makeProposalKey
  , addDataToSign
  , permitProtect
  , sendXtz
  , ProposalMetadataFromNum (..)
  , createSampleProposal
  ) where

import Universum

import qualified Data.Map as M
import Lorentz hiding ((>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest

import Ligo.BaseDAO.Types
import qualified Ligo.BaseDAO.Types as DAO

type OriginateFn param m = m ((Address, Address), (Address, Address), TAddress param, Address)

-- TODO: Since Ligo contract has different error messages (string type)
-- we need this to be able to catch error message properly.
type IsLorentz = Bool

-- | Used for check error from ligo contract
expectFailed
 :: forall caps base m
  . (MonadNettest caps base m)
  => Address -> MText -> m () -> m ()
expectFailed addr val = flip expectFailure (NettestFailedWith addr val)

totalSupplyFromLedger :: Ledger -> TotalSupply
totalSupplyFromLedger (BigMap ledger) =
  M.foldrWithKey (\(_, tokenId) val totalSup ->
      M.adjust ((+) val) tokenId totalSup
    )
    (M.fromList [(frozenTokenId, 0), (unfrozenTokenId, 0)])
    ledger

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: (MonadNettest caps base m, FA2.ParameterC param, HasCallStack)
  => FA2.TokenId -> TAddress param
  -> Address -> Natural
  -> m ()
checkTokenBalance tokenId dao addr expectedValue = withFrozenCallStack $ do
  consumer <- originateSimple "consumer" [] contractConsumer

  withSender (AddressResolved addr) $ call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = addr
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((addr, tokenId), expectedValue)]] )

makeProposalKey :: NicePackedValue pm => DAO.ProposeParams pm -> Address -> ProposalKey pm
makeProposalKey params owner = toHashHs $ lPackValue (params, owner)

addDataToSign
  :: (MonadNettest caps base m)
  => TAddress param
  -> Nonce
  -> d
  -> m (DataToSign d, d)
addDataToSign (toAddress -> dsContract) dsNonce dsData = do
  dsChainId <- getChainId
  return (DataToSign{..}, dsData)

-- | Add a permit from given user.
permitProtect
  :: (MonadNettest caps base m, NicePackedValue a)
  => AddressOrAlias -> (DataToSign a, a) -> m (PermitProtected a)
permitProtect author (toSign, a) = do
  authorAlias <- getAlias author
  pKey <- getPublicKey author
  pSignature <- signBinary (lPackValue toSign) authorAlias
  return PermitProtected
    { ppArgument = a
    , ppPermit = Just Permit{..}
    }

sendXtz
  :: (MonadNettest caps base m, HasCallStack, NiceParameter pm)
  => Address -> EpName -> pm -> m ()
sendXtz addr epName pm = withFrozenCallStack $ do
  let transferData = TransferData
        { tdTo = AddressResolved addr
        , tdAmount = toMutez 0.5_e6 -- 0.5 xtz
        , tdEntrypoint = epName
        , tdParameter = pm
        }
  transfer transferData

-- | Since in LIGO proposal metadata type is fixed but is inconvenient to work
-- with, we need a way to abstract away from that complexity - this problem
-- is resolved by the following typeclass.
class (KnownValue n, NicePackedValue n) => ProposalMetadataFromNum n where
  -- | Generate a proposal metadata.
  -- Different numbers must result in different values.
  proposalMetadataFromNum :: Int -> n

instance ProposalMetadataFromNum Integer where
  proposalMetadataFromNum = fromIntegral

createSampleProposal
  :: ( ProposalMetadataFromNum proposal
     , MonadNettest caps base m
     , ParameterContainsEntrypoints param '["Propose" :> ProposeParams proposal]
     , HasCallStack
     )
  => Int -> Address -> TAddress param -> m (ProposalKey proposal)
createSampleProposal counter owner1 dao = do
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum counter
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)

instance ProposalMetadataFromNum ProposalMetadataL where
  proposalMetadataFromNum n =
    one ([mt|int|], lPackValueRaw @Integer $ fromIntegral n)

-- TODO: Implement this via [#31] instead
-- checkIfAProposalExist
--   :: MonadNettest caps base m
--   => ProposalKey -> TAddress (Parameter TestProposalMetadata) -> m ()
-- checkIfAProposalExist proposalKey dao = do
--   owner :: Address <- newAddress "owner"
--   consumer <- originateSimple "consumer" [] contractConsumer
--   -- | If the proposal exists, there should be no error
--   callFrom (AddressResolved owner) dao (Call @"Proposal_metadata") (mkView proposalKey consumer)

-- TODO [#31]: See this ISSUES: https://gitlab.com/morley-framework/morley/-/issues/415#note_435327096
-- Check if certain field in storage
-- checkPropertyOfProposal :: _
-- checkPropertyOfProposal = error "undefined"
