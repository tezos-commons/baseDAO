-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.TreasuryDAO
  ( hprop_TreasuryDaoSMT
  ) where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Gen.Tezos.Address (genAddress)
import qualified Hedgehog.Range as Range

import Lorentz hiding (and, div, now, (>>))
import Morley.Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Contract (baseDAOTreasuryStorageLigo)
import Ligo.BaseDAO.TreasuryDAO.Types
import Ligo.BaseDAO.Types
import SMT.Common.Gen
import SMT.Common.Helper
import SMT.Common.Run
import SMT.Common.Types
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common (ContractType(..), makeProposalKey, metadataSize)
import Test.Ligo.TreasuryDAO.Types


hprop_TreasuryDaoSMT :: Property
hprop_TreasuryDaoSMT =
  let
    treasuryFs = #treasuryFs :! baseDAOTreasuryStorageLigo
    option = SmtOption
      { soMkPropose = genProposeTreasuryDao
      , soMkCustomCalls = genCustomCallsTreasuryDao
      , soModifyFs = addTreasuryDaoConfig treasuryFs
      , soContractType = TreasuryDaoContract

      , soProposalCheck = treasuryDaoProposalCheck
      , soRejectedProposalSlashValue = treasuryDaoRejectedProposalSlashValue
      , soDecisionLambda = treasuryDaoDecisionLambda
      , soCustomEps = \_ -> pure ()
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT @'Treasury option


addTreasuryDaoConfig :: ("treasuryFs" :! TreasuryFullStorage) -> TreasuryFullStorage -> TreasuryFullStorage
addTreasuryDaoConfig (Arg treasuryFs) fs =
  let treasuryStore = treasuryFs & fsStorage
      treasuryConfig = treasuryFs & fsConfig
  in fs
      { fsStorage = (fs & fsStorage)
          { sExtra = (sExtra treasuryStore)
              { teFrozenScaleValue = Just 1
              , teFrozenExtraValue = Just 0
              , teMaxProposalSize = Just 100
              , teSlashScaleValue = Just 1
              , teSlashDivisionValue = Just 1
              , teMinXtzAmount = Just 0
              , teMaxXtzAmount = Just 100
              }
          }
      , fsConfig = treasuryConfig
      }

-------------------------------------------------------------------------------
-- Lambdas
-------------------------------------------------------------------------------

treasuryDaoProposalCheck :: (ProposeParams, VariantToExtra 'Treasury) -> ModelT 'Treasury ()
treasuryDaoProposalCheck (params, extras) = do
  let proposalSize = metadataSize (params & ppProposalMetadata)

      frozenScaleValue = lookupWithError $ teFrozenScaleValue extras
      frozenExtraValue = lookupWithError $ teFrozenExtraValue extras
      maxProposalSize = lookupWithError $ teMaxProposalSize extras
      minXtzAmount = lookupWithError $ teMinXtzAmount extras
      maxXtzAmount = lookupWithError $ teMaxXtzAmount extras

      requiredTokenLock = frozenScaleValue * proposalSize + frozenExtraValue

  when
    (  (params & ppFrozenToken) /= requiredTokenLock
    || proposalSize >= maxProposalSize
    ) $ throwError FAIL_PROPOSAL_CHECK

  let metadata = (params & ppProposalMetadata)
        & lUnpackValueRaw @TreasuryDaoProposalMetadata
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
    Update_guardian _ -> pure ()
    Update_contract_delegate _ -> pure ()


treasuryDaoRejectedProposalSlashValue :: (Proposal, VariantToExtra 'Treasury) -> ModelT 'Treasury Natural
treasuryDaoRejectedProposalSlashValue (p, extras) = do

  let slashScaleValue = fromMaybe (error "MISSING_VALUE") $ teSlashScaleValue extras
      slashDivisionValue = fromMaybe (error "MISSING_VALUE") $ teSlashDivisionValue extras
  pure $ (slashScaleValue * (p & plProposerFrozenToken) `div` slashDivisionValue)

treasuryDaoDecisionLambda :: DecisionLambdaInput' (VariantToExtra 'Treasury) -> ModelT 'Treasury ([SimpleOperation], VariantToExtra 'Treasury, Maybe Address)
treasuryDaoDecisionLambda DecisionLambdaInput'{..} = do
  let metadata = (diProposal & plMetadata)
        & lUnpackValueRaw @TreasuryDaoProposalMetadata
        & fromRight (error "UNPACKING_PROPOSAL_METADATA_FAILED")

  case metadata of
    Transfer_proposal tp -> do
      let ops = foldl' handleTransfer [] (tp & tpTransfers)
      pure $ (ops, diExtra, Nothing)
    Update_guardian guardian ->
      pure $ ([], diExtra, Just guardian)
    Update_contract_delegate _ ->
      pure $ ([], diExtra, Nothing)

-------------------------------------------------------------------------------
-- Gen Functions
-------------------------------------------------------------------------------

genProposeTreasuryDao :: MkGenPropose 'Treasury
genProposeTreasuryDao senderInput delegate1 invalidFrom = do
  from <- Gen.element [senderInput, invalidFrom, delegate1]
  mkMetadata <- genTreasuryDaoProposalMetadata
  pure $ \guardian gov ->
    let metadata = mkMetadata guardian gov
        proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata metadata
        metaSize = metadataSize proposalMeta
        param = ProposeParams
          { ppFrom = from
          , ppFrozenToken = metaSize
          , ppProposalMetadata = proposalMeta
          }
        proposalKey = makeProposalKey param
    in (XtzAllowed $ ConcreteEp $ Propose param, metaSize, proposalKey)

genCustomCallsTreasuryDao :: MkGenCustomCalls 'Treasury
genCustomCallsTreasuryDao =
  pure []

genTransferProposal :: GeneratorT 'Treasury (Address -> Address -> TreasuryDaoProposalMetadata)
genTransferProposal = do
  agoraId <- Gen.integral (Range.constant 1 10)

  mkTranfers <- Gen.list (Range.linear 1 3) do
    Gen.choice [genXtzTransferType, genTokenTransferType]

  pure $ \_ gov ->
    let transfers = (\f -> f gov) <$> mkTranfers
    in Transfer_proposal $ TransferProposal
        { tpAgoraPostId  = agoraId
        , tpTransfers    = transfers
        }

genTreasuryDaoProposalMetadata :: GeneratorT 'Treasury (Address -> Address -> TreasuryDaoProposalMetadata)
genTreasuryDaoProposalMetadata = do
  guardianAddr <- genAddress
  Gen.choice [genTransferProposal, pure $ \_ _ -> Update_guardian guardianAddr]
