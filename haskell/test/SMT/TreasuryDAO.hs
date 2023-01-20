-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.TreasuryDAO
  ( hprop_TreasuryDaoSMT
  ) where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Tezos.Address (genAddress)
import Hedgehog.Range qualified as Range

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
      , soModifyS = addTreasuryDaoConfig treasuryFs
      , soContractType = TreasuryDaoContract

      , soProposalCheck = treasuryDaoProposalCheck
      , soRejectedProposalSlashValue = treasuryDaoRejectedProposalSlashValue
      , soDecisionCallback = treasuryDaoDecisionCallback
      , soCustomEps = \_ -> pure ()
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT @'Treasury option


addTreasuryDaoConfig :: ("treasuryFs" :! TreasuryStorage) -> TreasuryStorage -> TreasuryStorage
addTreasuryDaoConfig (Arg treasuryFs) fs =
  fs { sExtra = (sExtra treasuryFs)
              { teFrozenScaleValue = 1
              , teFrozenExtraValue = 0
              , teMaxProposalSize = 100
              , teSlashScaleValue = 1
              , teSlashDivisionValue = 1
              , teMinXtzAmount = 0
              , teMaxXtzAmount = 100
              }
          , sConfig = sConfig treasuryFs
          }

-------------------------------------------------------------------------------
-- Lambdas
-------------------------------------------------------------------------------

treasuryDaoProposalCheck :: (ProposeParams, VariantToExtra 'Treasury) -> ModelT 'Treasury ()
treasuryDaoProposalCheck (params, extras) = do
  let proposalSize = metadataSize (params & ppProposalMetadata)

      frozenScaleValue = teFrozenScaleValue extras
      frozenExtraValue = teFrozenExtraValue extras
      maxProposalSize = teMaxProposalSize extras
      minXtzAmount = teMinXtzAmount extras
      maxXtzAmount = teMaxXtzAmount extras

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
                      Legacy_token_transfer_type _ -> True
                      Xtz_transfer_type xt ->
                           (xt & xtAmount) >= minXtzAmount
                        && (xt & xtAmount) <= maxXtzAmount
                        && (xt & xtAmount) /= zeroMutez
                  )
              & and
      unless isValid $
        throwError FAIL_PROPOSAL_CHECK
    Update_guardian _ -> pure ()
    Update_contract_delegate _ -> pure ()


treasuryDaoRejectedProposalSlashValue :: (Proposal, VariantToExtra 'Treasury) -> ModelT 'Treasury Natural
treasuryDaoRejectedProposalSlashValue (p, extras) = do

  let slashScaleValue = teSlashScaleValue extras
      slashDivisionValue = teSlashDivisionValue extras
  pure $ (slashScaleValue * (p & plProposerFrozenToken) `div` slashDivisionValue)

treasuryDaoDecisionCallback :: DecisionCallbackInput' (VariantToExtra 'Treasury) -> ModelT 'Treasury ([SimpleOperation], VariantToExtra 'Treasury, Maybe Address)
treasuryDaoDecisionCallback DecisionCallbackInput'{..} = do
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
