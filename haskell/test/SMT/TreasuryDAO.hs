-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.TreasuryDAO
  ( hprop_TreasuryDaoSMT
  )  where

import Universum hiding (drop, swap)

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Gen.Tezos.Address (genAddress)
import Named (NamedF(..))

import Lorentz hiding (now, (>>), and)
import Michelson.Text (unsafeMkMText)

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import Ligo.Util
import SMT.Common.Gen
import SMT.Common.Helper
import SMT.Common.Run
import SMT.Common.Types
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common (makeProposalKey, metadataSize)
import Test.Ligo.TreasuryDAO.Types


hprop_TreasuryDaoSMT :: Property
hprop_TreasuryDaoSMT =
  let
    treasuryFs = fromVal ($(fetchValue @FullStorage "haskell/test/treasuryDAO_storage.tz" "TREASURY_STORAGE_PATH"))
    option = SmtOption
      { soMkPropose = genProposeTreasuryDao
      , soMkCustomCalls = genCustomCallsTreasuryDao
      , soModifyFs = addTreasuryDaoConfig treasuryFs
      , soContractType = TreasuryDaoContract

      , soProposalCheck = treasuryDaoProposalCheck
      , soRejectedProposalSlashValue = treasuryDaoRejectedProposalSlashValue
      , soDecisionLambda = treasuryDaoDecisionLambda
      , soCustomEps = Map.fromList [(unsafeMkMText "receive_xtz", \_ -> pure ())]
      }
  in
    withTests 30 $ property $ do
      runBaseDaoSMT option


addTreasuryDaoConfig :: ("treasuryFs" :! FullStorage) -> FullStorage -> FullStorage
addTreasuryDaoConfig (Arg treasuryFs) fs =
  let treasuryStore = treasuryFs & fsStorage
      treasuryConfig = treasuryFs & fsConfig
  in fs
      { fsStorage = (fs & fsStorage)
          { sExtra = treasuryStore & sExtra
          }
      , fsConfig = (fs & fsConfig)
          { cProposalCheck = treasuryConfig & cProposalCheck
          , cRejectedProposalSlashValue = treasuryConfig & cRejectedProposalSlashValue
          , cDecisionLambda = treasuryConfig & cDecisionLambda
          , cCustomEntrypoints = treasuryConfig & cCustomEntrypoints
          }
      }


-------------------------------------------------------------------------------
-- Lambdas
-------------------------------------------------------------------------------

treasuryDaoProposalCheck :: (ProposeParams, ContractExtra) -> ModelT ()
treasuryDaoProposalCheck (params, extras) = do
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


treasuryDaoRejectedProposalSlashValue :: (Proposal, ContractExtra) -> ModelT Natural
treasuryDaoRejectedProposalSlashValue (p, extras) = do
  let slashScaleValue = unpackWithError @Natural $ findBigMap "slash_scale_value" extras
      slashDivisionValue = unpackWithError @Natural $ findBigMap "slash_division_value" extras
  pure $ (slashScaleValue * (p & plProposerFrozenToken) `div` slashDivisionValue)

treasuryDaoDecisionLambda :: DecisionLambdaInput BigMap -> ModelT ([SimpleOperation], ContractExtra, Maybe Address)
treasuryDaoDecisionLambda DecisionLambdaInput{..} = do
  let metadata = (diProposal & plMetadata)
        & lUnpackValueRaw @TreasuryDaoProposalMetadata
        & fromRight (error "UNPACKING_PROPOSAL_METADATA_FAILED")

  case metadata of
    Transfer_proposal tp -> do
      let ops = foldl' handleTransfer [] (tp & tpTransfers)
      pure $ (ops, diExtra, Nothing)
    Update_guardian guardian ->
      pure $ ([], diExtra, Just guardian)


-------------------------------------------------------------------------------
-- Gen Functions
-------------------------------------------------------------------------------

genProposeTreasuryDao :: MkGenPropose
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
    in (XtzAllowed $ Propose param, metaSize, proposalKey)

genCustomCallsTreasuryDao :: MkGenCustomCalls
genCustomCallsTreasuryDao =
  pure [ \_ -> ([mt|receive_xtz|], lPackValueRaw ()) ]

genTransferProposal :: GeneratorT (Address -> Address -> TreasuryDaoProposalMetadata)
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

genTreasuryDaoProposalMetadata :: GeneratorT (Address -> Address -> TreasuryDaoProposalMetadata)
genTreasuryDaoProposalMetadata = do
  guardianAddr <- genAddress
  Gen.choice [genTransferProposal, pure $ \_ _ -> Update_guardian guardianAddr]

