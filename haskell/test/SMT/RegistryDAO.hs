-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.RegistryDAO
  ( hprop_RegistryDaoSMT
  ) where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Named (NamedF(..))

import Lorentz hiding (now, (>>), and)
import Michelson.Text (unsafeMkMText)
import qualified Michelson.Typed as T
import Hedgehog.Gen.Michelson (genMText)
import Hedgehog.Gen.Tezos.Address (genAddress)

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import Ligo.Util
import SMT.Common.Gen
import SMT.Common.Helper
import SMT.Common.Run
import SMT.Common.Types
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common (makeProposalKey, metadataSize)
import Test.Ligo.RegistryDAO.Types


hprop_RegistryDaoSMT :: Property
hprop_RegistryDaoSMT =
  let
    registryFs = fromVal ($(fetchValue @FullStorage "haskell/test/registryDAO_storage.tz" "REGISTRY_STORAGE_PATH"))
    option = SmtOption
      { soMkPropose = genProposeRegistryDao
      , soMkCustomCalls = genCustomCallsRegistryDao
      , soModifyFs = addRegistryDaoConfig registryFs
      , soContractType = RegistryDaoContract

      , soProposalCheck = registryDaoProposalCheck
      , soRejectedProposalSlashValue = registryDaoRejectedProposalSlashValue
      , soDecisionLambda = registryDaoDecisionLambda
      , soCustomEps = Map.fromList
          [ ( unsafeMkMText "receive_xtz", \_ -> pure () )
          , ( unsafeMkMText "lookup_registry", lookupRegistryEntrypoint)
          ]
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT option

addRegistryDaoConfig :: ("registryFs" :! FullStorage) -> FullStorage -> FullStorage
addRegistryDaoConfig (Arg registryFs) fs =
  let registryStore = registryFs & fsStorage
      registryConfig = registryFs & fsConfig
  in fs
      { fsStorage = (fs & fsStorage)
          { sExtra = registryStore & sExtra
          }
      , fsConfig = (fs & fsConfig)
          { cProposalCheck = registryConfig & cProposalCheck
          , cRejectedProposalSlashValue = registryConfig & cRejectedProposalSlashValue
          , cDecisionLambda = registryConfig & cDecisionLambda
          , cCustomEntrypoints = registryConfig & cCustomEntrypoints
          }
      }


-------------------------------------------------------------------------------
-- Lambdas
-------------------------------------------------------------------------------

registryDaoProposalCheck :: (ProposeParams, ContractExtra) -> ModelT ()
registryDaoProposalCheck (params, extras) = do
  let proposalSize = metadataSize (params & ppProposalMetadata)
      frozenScaleValue = unpackWithError @Natural $ findBigMap "frozen_scale_value" extras
      frozenExtraValue = unpackWithError @Natural $ findBigMap "frozen_extra_value" extras
      maxProposalSize = unpackWithError @Natural $ findBigMap "max_proposal_size" extras
      minXtzAmount = unpackWithError @Mutez $ findBigMap "min_xtz_amount" extras
      maxXtzAmount = unpackWithError @Mutez $ findBigMap "max_xtz_amount" extras
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
                        && (xt & xtAmount) /= (toMutez 0)
                  )
              & and
      unless isValid $
        throwError FAIL_PROPOSAL_CHECK
    Update_receivers_proposal _ -> pure ()
    Configuration_proposal _ -> pure ()
    Update_guardian _ -> pure ()


registryDaoRejectedProposalSlashValue :: (Proposal, ContractExtra) -> ModelT Natural
registryDaoRejectedProposalSlashValue (p, extras) = do
  let slashScaleValue = unpackWithError @Natural $ findBigMap "slash_scale_value" extras
      slashDivisionValue = unpackWithError @Natural $ findBigMap "slash_division_value" extras

  pure $ (slashScaleValue * (p & plProposerFrozenToken) `div` slashDivisionValue)

registryDaoDecisionLambda :: DecisionLambdaInput BigMap -> ModelT ([SimpleOperation], ContractExtra, Maybe Address)
registryDaoDecisionLambda DecisionLambdaInput{..} = do
  let metadata = (diProposal & plMetadata)
        & lUnpackValueRaw @RegistryDaoProposalMetadata
        & fromRight (error "UNPACKING_PROPOSAL_METADATA_FAILED")
      proposeParam = ProposeParams (diProposal & plProposer) (diProposal & plProposerFrozenToken) (diProposal & plMetadata)
      proposalKey = makeProposalKey (proposeParam)

  case metadata of
    Update_receivers_proposal urp -> do
      let currentSet = unpackWithError @(Set Address) $ findBigMap "proposal_receivers" diExtra
          updatedReceivers = case urp of
            Add_receivers receivers ->
              foldl' (\cSet addr -> Set.insert addr cSet) currentSet receivers
            Remove_receivers receivers ->
              foldl' (\cSet addr -> Set.delete addr cSet) currentSet receivers
          newExtra = Map.insert "proposal_receivers" (lPackValueRaw @(Set Address) updatedReceivers) (T.unBigMap $ unDynamic diExtra)
      pure ([], DynamicRec' $ BigMap $ newExtra, Nothing)
    Configuration_proposal cp -> do
      let newExtras = (T.unBigMap $ unDynamic diExtra)
            & (\ce -> case (cp & cpFrozenScaleValue) of
                  Just val -> Map.insert "frozen_scale_value" (lPackValueRaw @Natural val) ce
                  Nothing -> ce)
            & (\ce -> case (cp & cpFrozenExtraValue) of
                  Just val -> Map.insert "frozen_extra_value" (lPackValueRaw @Natural val) ce
                  Nothing -> ce)
            & (\ce -> case (cp & cpMaxProposalSize) of
                  Just val -> Map.insert "max_proposal_size" (lPackValueRaw @Natural val) ce
                  Nothing -> ce)
            & (\ce -> case (cp & cpSlashScaleValue) of
                  Just val -> Map.insert "slash_scale_value" (lPackValueRaw @Natural val) ce
                  Nothing -> ce)
            & (\ce -> case (cp & cpSlashDivisionValue) of
                  Just val -> Map.insert "slash_division_value" (lPackValueRaw @Natural val) ce
                  Nothing -> ce)
      pure $ ([], DynamicRec' $ BigMap $ newExtras, Nothing)
    Transfer_proposal tp -> do
      let extras = diExtra
            & applyDiff (tp & tpRegistryDiff)
            & applyDiffAffected proposalKey (tp & tpRegistryDiff)

      let ops = foldl' handleTransfer [] (tp & tpTransfers)
      pure $ (ops, extras, Nothing)
    Update_guardian guardian ->
      pure $ ([], diExtra, Just guardian)

applyDiffAffected :: ProposalKey -> [(MText, Maybe MText)] -> ContractExtra -> ContractExtra
applyDiffAffected proposalKey diffs ce =
  let registryAff = applyDiffRegistryAffected proposalKey diffs $ unpackWithError @(Map MText ProposalKey) $ findBigMap "registry_affected" ce
  in DynamicRec' $ BigMap $
      Map.insert "registry_affected" (lPackValueRaw @(Map MText ProposalKey) registryAff) (T.unBigMap $ unDynamic ce)

applyDiffRegistryAffected :: ProposalKey -> [(MText, Maybe MText)] -> Map MText ProposalKey -> Map MText ProposalKey
applyDiffRegistryAffected proposalKey diffs registryAff =
  foldl'
    (\regAff (key, _) ->
        Map.insert key proposalKey regAff
    ) registryAff diffs


applyDiff :: [(MText, Maybe MText)] -> ContractExtra -> ContractExtra
applyDiff diffs ce =
  let newRegistry = applyDiffRegistry diffs $ unpackWithError @(Map MText MText) $ findBigMap "registry" ce
  in DynamicRec' $ BigMap $
      Map.insert "registry" (lPackValueRaw @(Map MText MText) newRegistry) (T.unBigMap $ unDynamic ce)

applyDiffRegistry :: [(MText, Maybe MText)] -> Map MText MText -> Map MText MText
applyDiffRegistry diffs registry =
  foldl'
    (\reg (key, value) ->
        case value of
          Just v -> Map.insert key v reg
          Nothing -> Map.delete key reg
    ) registry diffs

-------------------------------------------------------------------------------
-- Custom entrypoints
-------------------------------------------------------------------------------

lookupRegistryEntrypoint :: ByteString -> ModelT ()
lookupRegistryEntrypoint packedParam  = do
  store <- getStore
  let (unpackVal :: Either ModelError (MText, Address, Map MText MText)) = do
        (key, addr) <- first (const UNPACKING_FAILED) $ lUnpackValueRaw @LookupRegistryParam packedParam

        packedRegistry <- case Map.lookup "registry" (store & sExtra & unDynamic & T.unBigMap ) of
          Just val -> Right val
          Nothing -> Left MISSING_VALUE
        registry <- first (const UNPACKING_FAILED) $ lUnpackValueRaw @(Map MText MText) packedRegistry
        Right (key, addr, registry)
  case unpackVal of
    Right (key, addr, registry) -> do
      let val = Map.lookup key registry
      let consumerParam :: (MText, (Maybe MText)) = (key, val)

      execOperation $ OtherOperation addr (show consumerParam) (toMutez 0)

    Left err -> throwError err


-------------------------------------------------------------------------------
-- Gen Functions
-------------------------------------------------------------------------------

genProposeRegistryDao :: MkGenPropose
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
    in (XtzAllowed $ Propose param, metaSize, proposalKey)

genCustomCallsRegistryDao :: MkGenCustomCalls
genCustomCallsRegistryDao = do
  mkLookupRegistryParam <- genLookupRegistryParam
  pure
    [ \_ -> ([mt|receive_xtz|], lPackValueRaw ())
    , \ModelInputArg{..} -> ([mt|lookup_registry|], lPackValueRaw @LookupRegistryParam (mkLookupRegistryParam miaViewContractAddr))
    ]

genLookupRegistryParam :: GeneratorT (TAddress (MText, Maybe MText) -> LookupRegistryParam)
genLookupRegistryParam = do
  key <- genMText
  pure $ \viewContractAddr -> (key, (unTAddress viewContractAddr))

genRegistryDaoProposalMetadata :: GeneratorT (Address -> Address -> RegistryDaoProposalMetadata)
genRegistryDaoProposalMetadata = do
  guardAddr <- genAddress
  Gen.choice
    [ genConfigProposal, genTransferProposal, genUpdateReceiverParam
    , pure $ \_ _ -> Update_guardian guardAddr
    ]

genConfigProposal :: GeneratorT (Address -> Address -> RegistryDaoProposalMetadata)
genConfigProposal = do
  valMaybe <- Gen.maybe $ Gen.integral (Range.constant 1 10)
  pure $ \_ _ -> Configuration_proposal $ ConfigProposal
    { cpFrozenScaleValue = valMaybe
    , cpFrozenExtraValue = valMaybe
    , cpSlashScaleValue = valMaybe
    , cpSlashDivisionValue = valMaybe
    , cpMaxProposalSize = valMaybe
    }

genTransferProposal :: GeneratorT (Address -> Address -> RegistryDaoProposalMetadata)
genTransferProposal = do
  agoraId <- Gen.integral (Range.constant 1 10)
  registryDiff <- Gen.list (Range.linear 1 3) do
    registryKey <- genMText
    registryValue <- Gen.maybe genMText
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

genUpdateReceiverParam :: GeneratorT (Address -> Address -> RegistryDaoProposalMetadata)
genUpdateReceiverParam = do
  amt <- Gen.integral (Range.constant 1 5)
  updateParams <-  Gen.list (Range.linear 1 amt) genAddress
  removeParams <-  Gen.list (Range.linear 1 amt) genAddress
  let adds = Add_receivers updateParams
  let removes = Remove_receivers removeParams
  updateReceiverP <- Gen.element [adds, removes]
  pure $ \_ _ -> Update_receivers_proposal $ updateReceiverP
