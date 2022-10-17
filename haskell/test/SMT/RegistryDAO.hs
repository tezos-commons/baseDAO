-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.RegistryDAO
  ( hprop_RegistryDaoSMT
  ) where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Gen.Michelson (genMText)
import Hedgehog.Gen.Tezos.Address (genAddress)
import Lorentz hiding (and, div, now, (>>))
import Morley.Michelson.Typed.Haskell.Value
import Morley.Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Contract (baseDAORegistryStorageLigo)
import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.Types
import SMT.Common.Gen
import SMT.Common.Helper
import SMT.Common.Run
import SMT.Common.Types
import SMT.Model.BaseDAO.Types
import Test.Cleveland.Lorentz.Types
import Test.Ligo.BaseDAO.Common (ContractType(..), makeProposalKey, metadataSize)
import Test.Ligo.RegistryDAO.Types

hprop_RegistryDaoSMT :: Property
hprop_RegistryDaoSMT =
  let
    registryFs = #registryFs :! baseDAORegistryStorageLigo
    option = SmtOption
      { soMkPropose = genProposeRegistryDao
      , soMkCustomCalls = genCustomCallsRegistryDao
      , soModifyS = addRegistryDaoConfig registryFs
      , soContractType = RegistryDaoContract

      , soProposalCheck = registryDaoProposalCheck
      , soRejectedProposalSlashValue = registryDaoRejectedProposalSlashValue
      , soDecisionCallback = registryDaoDecisionCallback
      , soCustomEps = \case
          Lookup_registry p -> lookupRegistryEntrypoint p
          _ -> pass
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT @'Registry option

addRegistryDaoConfig :: ("registryFs" :! RegistryStorage) -> RegistryStorage -> RegistryStorage
addRegistryDaoConfig (Arg registryFs) fs =
  let registryStore = registryFs
  in fs { sExtra = (sExtra registryStore)
              { reFrozenScaleValue = 1
              , reFrozenExtraValue = 0
              , reMaxProposalSize = 100
              , reSlashScaleValue = 1
              , reSlashDivisionValue = 1
              , reMinXtzAmount = 0
              , reMaxXtzAmount = 100
              }
          }

-------------------------------------------------------------------------------
-- Lambdas
-------------------------------------------------------------------------------

registryDaoProposalCheck :: (ProposeParams, VariantToExtra 'Registry) -> ModelT 'Registry ()
registryDaoProposalCheck (params, extras) = do
  let proposalSize = metadataSize (params & ppProposalMetadata)
      frozenScaleValue = reFrozenScaleValue extras
      frozenExtraValue = reFrozenExtraValue extras
      maxProposalSize = reMaxProposalSize extras
      minXtzAmount = reMinXtzAmount extras
      maxXtzAmount = reMaxXtzAmount extras
      requiredTokenLock = frozenScaleValue * proposalSize + frozenExtraValue

  when
    (  (params & ppFrozenToken) /= requiredTokenLock
    || proposalSize >= maxProposalSize
    ) $ throwError FAIL_PROPOSAL_CHECK

  let metadata = (params & ppProposalMetadata)
        & lUnpackValueRaw @RegistryDaoProposalMetadata
        & fromRight (error "UNPACKING_PROPOSAL_METADATA_FAILED")

  case metadata of
    Transfer_proposal tp -> do
      let isValid = (tp & tpTransfers)
              <&> (\case
                      Token_transfer_type _ -> True
                      Xtz_transfer_type xt ->
                           (xt & xtAmount) >= minXtzAmount
                        && (xt & xtAmount) <= maxXtzAmount
                        && (xt & xtAmount) /= zeroMutez
                  )
              & and
      unless isValid $
        throwError FAIL_PROPOSAL_CHECK
    Update_receivers_proposal _ -> pure ()
    Configuration_proposal _ -> pure ()
    Update_guardian _ -> pure ()
    Update_contract_delegate _ -> pure ()

registryDaoRejectedProposalSlashValue :: (Proposal, VariantToExtra 'Registry) -> ModelT 'Registry Natural
registryDaoRejectedProposalSlashValue (p, extras) = do
  let slashScaleValue = reSlashScaleValue extras
      slashDivisionValue = reSlashDivisionValue extras

  pure $ (slashScaleValue * (p & plProposerFrozenToken) `div` slashDivisionValue)

registryDaoDecisionCallback :: DecisionCallbackInput' (VariantToExtra 'Registry) -> ModelT cep ([SimpleOperation], VariantToExtra 'Registry, Maybe Address)
registryDaoDecisionCallback DecisionCallbackInput' {..} = do
  let metadata = (diProposal & plMetadata)
        & lUnpackValueRaw @RegistryDaoProposalMetadata
        & fromRight (error "UNPACKING_PROPOSAL_METADATA_FAILED")
      proposeParam = ProposeParams (diProposal & plProposer) (diProposal & plProposerFrozenToken) (diProposal & plMetadata)
      proposalKey = makeProposalKey (proposeParam)

  case metadata of
    Update_receivers_proposal urp -> do
      let currentSet = reProposalReceivers diExtra
          updatedReceivers = case urp of
            Add_receivers receivers ->
              foldl' (\cSet addr -> Set.insert addr cSet) currentSet receivers
            Remove_receivers receivers ->
              foldl' (\cSet addr -> Set.delete addr cSet) currentSet receivers
          newExtra = diExtra { reProposalReceivers = updatedReceivers }
      pure ([], newExtra, Nothing)
    Configuration_proposal cp -> do
      let newExtras = diExtra
            & (\ce -> case (cp & cpFrozenScaleValue) of
                  Just val -> ce { reFrozenScaleValue = val }
                  Nothing -> ce)
            & (\ce -> case (cp & cpFrozenExtraValue) of
                  Just val -> ce { reFrozenExtraValue = val }
                  Nothing -> ce)
            & (\ce -> case (cp & cpMaxProposalSize) of
                  Just val -> ce { reMaxProposalSize = val }
                  Nothing -> ce)
            & (\ce -> case (cp & cpSlashScaleValue) of
                  Just val -> ce { reSlashScaleValue = val }
                  Nothing -> ce)
            & (\ce -> case (cp & cpSlashDivisionValue) of
                  Just val -> ce { reSlashDivisionValue = val }
                  Nothing -> ce)
      pure $ ([], newExtras, Nothing)
    Transfer_proposal tp -> do
      let extras =
            applyDiffAffected proposalKey (tp & tpRegistryDiff) $ applyDiff (tp & tpRegistryDiff) $ diExtra

      let ops = foldl' handleTransfer [] (tp & tpTransfers)
      pure $ (ops, extras, Nothing)
    Update_guardian guardian ->
      pure $ ([], diExtra, Just guardian)
    Update_contract_delegate _ ->
      pure $ ([], diExtra, Nothing)

applyDiffAffected :: ProposalKey -> [(MText, Maybe MText)] -> (VariantToExtra 'Registry) -> (VariantToExtra 'Registry)
applyDiffAffected proposalKey diffs ce =
  let registryAff = applyDiffRegistryAffected proposalKey diffs $ reRegistryAffected ce
  in  ce { reRegistryAffected = registryAff }

applyDiffRegistryAffected :: ProposalKey -> [(MText, Maybe MText)] -> BigMap MText ProposalKey -> BigMap MText ProposalKey
applyDiffRegistryAffected proposalKey diffs registryAff =
  foldl'
    (\regAff (key, _) ->
        insertToBigMap regAff key proposalKey
    ) registryAff diffs


applyDiff :: [(MText, Maybe MText)] -> (VariantToExtra 'Registry) -> (VariantToExtra 'Registry)
applyDiff diffs ce =
  let newRegistry = applyDiffRegistry diffs $ reRegistry ce
  in ce { reRegistry =  newRegistry }

applyDiffRegistry :: [(MText, Maybe MText)] -> BigMap MText MText -> BigMap MText MText
applyDiffRegistry diffs registry =
  foldl'
    (\reg (key, value) ->
        case value of
          Just v -> insertToBigMap reg key v
          Nothing -> deleteFromBigMap reg key
    ) registry diffs

insertToBigMap :: Ord k => BigMap k v -> k -> v -> BigMap k v
insertToBigMap (BigMap bid m) k v = BigMap bid (Map.insert k v m)

deleteFromBigMap :: Ord k => BigMap k v -> k -> BigMap k v
deleteFromBigMap (BigMap bid m) k = BigMap bid (Map.delete k m)

lookupInBigMap :: Ord k => BigMap k v -> k -> Maybe v
lookupInBigMap (BigMap _ m) k = Map.lookup k m

-------------------------------------------------------------------------------
-- Custom entrypoints
-------------------------------------------------------------------------------

lookupRegistryEntrypoint :: LookupRegistryParam -> ModelT 'Registry ()
lookupRegistryEntrypoint (LookupRegistryParam key addr)  = do
  store <- getStore @'Registry
  let registry = reRegistry $ sExtra $ store
  let val = lookupInBigMap registry key
  let consumerParam :: (MText, (Maybe MText)) = (key, val)
  execOperation $ OtherOperation addr (show consumerParam) zeroMutez


-------------------------------------------------------------------------------
-- Gen Functions
-------------------------------------------------------------------------------

genProposeRegistryDao :: MkGenPropose 'Registry
genProposeRegistryDao senderInput delegate1 invalidFrom = do
  from <- Gen.element [senderInput, invalidFrom, delegate1]
  mkMetadata <- genRegistryDaoProposalMetadata
  pure $ \guardian gov ->
    let metadata = mkMetadata guardian gov
        proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata metadata
        metaSize = metadataSize proposalMeta
        param = ProposeParams
          { ppFrom = from
          , ppFrozenToken = metaSize
          , ppProposalMetadata = proposalMeta
          }
        proposalKey = makeProposalKey param
    in (XtzAllowed $ ConcreteEp $ Propose param, metaSize, proposalKey)

genCustomCallsRegistryDao :: MkGenCustomCalls 'Registry
genCustomCallsRegistryDao = do
  mkLookupRegistryParam <- genLookupRegistryParam
  pure
    [ \ModelInputArg{..} ->
          Lookup_registry (mkLookupRegistryParam miaViewContractAddr)
    ]

genLookupRegistryParam :: GeneratorT 'Registry (ContractHandle (MText, Maybe MText) s () -> LookupRegistryParam)
genLookupRegistryParam = do
  key <- genMText def
  pure $ \viewContractAddr -> (LookupRegistryParam key (toAddress viewContractAddr))

genRegistryDaoProposalMetadata :: GeneratorT 'Registry (Address -> Address -> RegistryDaoProposalMetadata)
genRegistryDaoProposalMetadata = do
  guardAddr <- genAddress
  Gen.choice
    [ genConfigProposal, genTransferProposal, genUpdateReceiverParam
    , pure $ \_ _ -> Update_guardian guardAddr
    ]

genConfigProposal :: GeneratorT 'Registry (Address -> Address -> RegistryDaoProposalMetadata)
genConfigProposal = do
  valMaybe <- Gen.maybe $ Gen.integral (Range.constant 1 10)
  pure $ \_ _ -> Configuration_proposal $ ConfigProposal
    { cpFrozenScaleValue = valMaybe
    , cpFrozenExtraValue = valMaybe
    , cpSlashScaleValue = valMaybe
    , cpSlashDivisionValue = valMaybe
    , cpMaxProposalSize = valMaybe
    }

genTransferProposal :: GeneratorT 'Registry (Address -> Address -> RegistryDaoProposalMetadata)
genTransferProposal = do
  agoraId <- Gen.integral (Range.constant 1 10)
  registryDiff <- Gen.list (Range.linear 1 3) do
    registryKey <- genMText def
    registryValue <- Gen.maybe (genMText def)
    pure (registryKey, registryValue)

  mkTranfers <- Gen.list (Range.linear 1 3) do
    Gen.choice [genXtzTransferType, genTokenTransferType]

  pure $ \_ gov ->
    let transfers = (\f -> f gov) <$> mkTranfers
    in Transfer_proposal $ TransferProposal
        { tpAgoraPostId  = agoraId
        , tpTransfers    = transfers
        , tpRegistryDiff = registryDiff
        }

genUpdateReceiverParam :: GeneratorT 'Registry (Address -> Address -> RegistryDaoProposalMetadata)
genUpdateReceiverParam = do
  amt <- Gen.integral (Range.constant 1 5)
  updateParams <-  Gen.list (Range.linear 1 amt) genAddress
  removeParams <-  Gen.list (Range.linear 1 amt) genAddress
  let adds = Add_receivers updateParams
  let removes = Remove_receivers removeParams
  updateReceiverP <- Gen.element [adds, removes]
  pure $ \_ _ -> Update_receivers_proposal $ updateReceiverP
