-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

-- | Generate the storage and entrypoint call
module SMT.Common.Gen
  ( genMkModelInput
  , genXtzTransferType
  , genTokenTransferType
  ) where

import Prelude hiding (drop, swap)

import Crypto.Random (drgNewSeed, seedFromInteger, withDRG)
import Data.Map qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Tezos.Address (genAddress)
import Hedgehog.Range qualified as Range

import Hedgehog.Gen.Tezos.Crypto (genSecretKey)
import Lorentz hiding (cast, concat, get, not)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed.Haskell.Value (BigMap(..))
import Morley.Tezos.Address (Address(..), mkKeyAddress, parseContractHash)
import Morley.Tezos.Core (dummyChainId)
import Morley.Tezos.Crypto (SecretKey, sign, toPublic)
import Morley.Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import SMT.Common.Types
import SMT.Model.BaseDAO.Types

genMkModelInput :: Default (VariantToExtra var) => SmtOption var -> GeneratorT var (MkModelInput var)
genMkModelInput option@SmtOption{..} = do

  -- Initial
  addrs <- genSecretKey
    <&> (\secret -> (mkKeyAddress . toPublic $ secret, secret))
    & vectorOf poolSize
  startLevel <- Gen.integral (Range.constant 0 10)

  modify (\s -> s { gsAddresses = addrs, gsLevel = startLevel, gsMkGenPropose = soMkPropose, gsMkCustomCalls = soMkCustomCalls })

  -- Run Gen
  mkContractCalls <- replicateM groupCallAmount genContractCallGroup

  mkModelState <- genModelState option

  pure $ \args@ModelInputArg{..} -> ModelInput
    ( mkContractCalls
         &  concat
        <&> (\f -> f args)
    , mkModelState miaGuardianAddr miaGovAddr)
  where
    groupCallAmount = 5

-- | Generate a list of BaseDAO contract calls that works together.
genContractCallGroup :: GeneratorT cep ([ModelInputArg -> ModelCall cep])
genContractCallGroup = do
  Gen.choice [genProposingProcess, genRandomCalls]


genStorage :: Default (VariantToExtra var) => GeneratorT var (Address -> Address -> StorageSkeleton (VariantToExtra var))
genStorage = do
  config <- genConfig
  userPool <- get <&> gsAddresses
  admin <- userPool <&> fst & Gen.element

  delegates <- genDelegates

  startLevel <- get <&> gsLevel
  freezeHistory <- genFreezeHistory

  let staked = getStaked freezeHistory
  quorumThresholdAtCycle <- genQuorumThresholdAtCycle staked

  pure $ \guardAddr govAddr -> Storage
        { sFrozenTotalSupply = getTotalSupply freezeHistory
        , sDelegates = BigMap Nothing delegates
        , sFreezeHistory = BigMap Nothing freezeHistory
        , sAdmin = admin
        , sGuardian = guardAddr
        , sGovernanceToken = GovernanceToken
              { gtAddress = govAddr
              , gtTokenId = FA2.theTokenId
              }
        , sStartLevel = startLevel
        , sQuorumThresholdAtCycle = quorumThresholdAtCycle
        , sProposals = BigMap Nothing mempty
        , sOngoingProposalsDlist = Nothing
        , sStakedVotes = BigMap Nothing mempty
        , sExtra = def

        , sFrozenTokenId = FA2.theTokenId
        , sMetadata = mempty
        , sPendingOwner = admin
        , sPermitsCounter = Nonce 0
        , sConfig = config
        }

genConfig :: GeneratorT cep Config
genConfig = do
  let votingPeriod = Period 1
  let fixedProposalFee = FixedFee fixedFee
  let maxChangePercent = 0
  let changePercent = 19
  let governanceTotalSupply = 500
  pure $ Config
    { cFixedProposalFee = fixedProposalFee
    , cPeriod = votingPeriod
    , cProposalFlushLevel = (unPeriod votingPeriod) * 2
    , cProposalExpiredLevel = (unPeriod votingPeriod) * 3
    , cMaxQuorumChange = QuorumFraction $ percentageToFractionNumerator maxChangePercent
    , cQuorumChange = QuorumFraction $ percentageToFractionNumerator changePercent
    , cGovernanceTotalSupply = governanceTotalSupply
    , cMaxQuorumThreshold = percentageToFractionNumerator 99 -- 99%
    , cMinQuorumThreshold = percentageToFractionNumerator 1 -- 1%
    }

genDelegates :: GeneratorT cep (Map Delegate ())
genDelegates = do
  userPool <- get <&> gsAddresses <<&>> fst
  delegateList <- (Gen.shuffle $ (uncurry Delegate) <$> zip userPool userPool) >>= Gen.subsequence
  pure $ Map.fromList $ zip delegateList $ repeat ()

genDelegateParams :: GeneratorT cep [DelegateParam]
genDelegateParams = do
  userPool <- get <&> gsAddresses <<&>> fst
  isEnable <- Gen.bool
  userPool
    <&> (\addr -> DelegateParam isEnable addr)
     &  Gen.shuffle
    >>= Gen.subsequence

genAddressFreezeHistory :: GeneratorT cep AddressFreezeHistory
genAddressFreezeHistory = do
  lvl <- get <&> gsLevel
  currentUnstaked <- Gen.integral (Range.constant 0 30)
  pastUnstaked <- Gen.integral (Range.constant 0 40)
  staked <- Gen.integral (Range.constant 0 50)
  pure $ AddressFreezeHistory
        { fhCurrentUnstaked = currentUnstaked
        , fhPastUnstaked = pastUnstaked
        , fhCurrentStageNum = lvl
        , fhStaked = staked
        }

genFreezeHistory :: GeneratorT cep (Map Address AddressFreezeHistory)
genFreezeHistory = do
  addrs <- get <&> gsAddresses <<&>> fst
  addrFreezeHistoryList <- vectorOf poolSize genAddressFreezeHistory
  let freezeHistory = zip addrs addrFreezeHistoryList
  pure $ Map.fromList freezeHistory

genQuorumThresholdAtCycle :: Natural -> GeneratorT cep QuorumThresholdAtCycle
genQuorumThresholdAtCycle staked = do
  quorumThreshold <- Gen.integral (Range.constant 1 3)
  lvl <- get <&> gsLevel
  pure $ QuorumThresholdAtCycle quorumThreshold lvl staked

genModelState :: Default (VariantToExtra var) => SmtOption var -> GeneratorT var (Address -> Address -> ModelState var)
genModelState SmtOption{..} = do

  mkStore <- genStorage
  let mkFs = \guardian gov -> mkStore guardian gov

  selfAddrPlaceholder <- genAddress
  pure $ \guardian gov ->
    let fs = mkFs guardian gov
    in ModelState
        { msStorage = fs
        , msMutez = zeroMutez

        , msLevel = fs & sStartLevel
        , msChainId = dummyChainId
        , msSelfAddress = selfAddrPlaceholder -- This will be replace when dao is originated
        , msGovernanceTokenAddress = gov
        , msContracts = mempty
        , msProposalCheck = soProposalCheck
        , msRejectedProposalSlashValue = soRejectedProposalSlashValue
        , msDecisionCallback = soDecisionCallback
        , msCustomEps = soCustomEps
        }

-------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------

-- | Generate completely random calls
genRandomCalls :: forall cep. GeneratorT cep ([ModelInputArg -> ModelCall cep])
genRandomCalls = do
  addrs <- get <&> gsAddresses <<&>> fst
  sender1 <- Gen.element addrs

  -- gen entrypoints
  calls <- genProposingProcess @cep
  updateDelegateCall <- genUpdateDelegate <&> mkSimpleContractCall sender1
  transferOwnershipCall <- genTransferOwnership <&> mkSimpleContractCall sender1
  acceptOwnershipCall <- genAcceptOwnership <&> mkSimpleContractCall sender1
  mkTransferContractCall <- genTransferContractTokensCall sender1

  pure $
    [ \_ -> updateDelegateCall
    , \_ ->transferOwnershipCall
    , \_ -> acceptOwnershipCall
    , \ModelInputArg{..} -> mkTransferContractCall miaGovAddr
    ] <> calls
  >>= Gen.shuffle
  >>= Gen.subsequence

  where
    genTransferContractTokensCall :: Address -> GeneratorT cep (Address -> ModelCall cep)
    genTransferContractTokensCall sender1 = do
      f <- genTransferContractTokens @cep
      pure $ \govAddr -> mkSimpleContractCall sender1 $ f govAddr


mkSimpleContractCall :: Address -> Parameter' (VariantToParam var) -> ModelCall var
mkSimpleContractCall sender1 param = do
  ModelCall
    { mcAdvanceLevel = Nothing
    , mcParameter = param
    , mcSource = ModelSource
        { msoSource = sender1
        , msoSender = sender1
        }
    }

-- TODO [#295]: Generated entrypoint calls are too rigid.
genProposingProcess :: GeneratorT cep ([ModelInputArg -> ModelCall cep])
genProposingProcess = do
  userAddrs <- get <&> gsAddresses
  sender1 <- fst <$> Gen.element userAddrs
  delegate1 <- fst <$> Gen.element userAddrs
  invalidFrom <- fst <$> Gen.element userAddrs

  mkGenPropose <- get <&> gsMkGenPropose
  mkPropose <- mkGenPropose sender1 delegate1 invalidFrom

  mkGenCustomCalls <- get <&> gsMkCustomCalls
  mkCustomCalls <- mkGenCustomCalls

  -- gen entrypoints
  flushAction <- genFlush
  mkVote <- genVote
  mkDropProposal <- genDropProposal
  mkUnstakeVote <- genUnstakeVote

  -- | `advanceLvl` is provided precisely to ensure the order of operations results in successful proposal.
  let
    mkCall :: Parameter' (VariantToParam var) -> Maybe Natural -> ModelCall var
    mkCall param advanceLvl = ModelCall
        { mcAdvanceLevel = advanceLvl
        , mcParameter = param
        , mcSource = ModelSource
          { msoSource = sender1
          , msoSender = sender1
          }
        }

  let applyArgs ModelInputArg{..} =
        let
          (proposeAction, metaSize, key) = mkPropose miaGuardianAddr miaGovAddr
          freezeAmt = metaSize + fixedFee
          permitProtecteds = mkVote key freezeAmt
          voterFreezeAmt = getVoteFreezeAmt permitProtecteds
          dropProposalAction = mkDropProposal key
          unstakeVoteAction = mkUnstakeVote key
        in (freezeAmt, voterFreezeAmt, permitProtecteds, proposeAction, dropProposalAction, unstakeVoteAction)

  pure $
      [ \_ -> mkCall (XtzForbidden $ Update_delegate [DelegateParam True delegate1]) Nothing
      , \args ->
          let (freezeAmt, _, _, _, _, _) = applyArgs args
          in mkCall (XtzForbidden $ Freeze (#amount :! (freezeAmt)) ) Nothing
      , \args ->
          let (_, voterFreezeAmt, _, _, _, _) = applyArgs args
          in mkCall (XtzForbidden $ Freeze (#amount :! voterFreezeAmt) ) Nothing
      , \args ->
          let (_, _, _, proposeAction, _, _) = applyArgs args
          in mkCall proposeAction (Just 1)
      , \args ->
          let (_, _, permitProtecteds, _, _, _) = applyArgs args
          in mkCall (XtzForbidden $ Vote permitProtecteds) (Just 1)
      , \_ -> mkCall flushAction (Just 2)
      , \args ->
          let (_, _, _, _, _, unstakeVoteAction) = applyArgs args
          in mkCall unstakeVoteAction Nothing
      , \args ->
          let (freezeAmt, _, _, _, _, _) = applyArgs args
          in mkCall (XtzForbidden $ Unfreeze (#amount :! (freezeAmt)) ) Nothing
      , \args ->
          let (_, _, _, _, dropProposalAction, _) = applyArgs args
          in mkCall dropProposalAction Nothing
      ]
       <> (

         mkCustomCalls
           <&> (\f args -> mkCall (XtzAllowed $ CustomEp $ f args) Nothing)
       )
  where
    getVoteFreezeAmt permitProtecteds =
      foldl' (\i (PermitProtected voteParam _) -> (voteParam & vVoteAmount) + i) 0 permitProtecteds

genVote :: GeneratorT cep (ProposalKey -> Natural -> [PermitProtected VoteParam])
genVote = do
  mkGenVoteParams <- vectorOf 5 $ genVoteParams
  pure $ \proposalKey correctVoteAmt -> (\f -> f proposalKey correctVoteAmt) <$> mkGenVoteParams
  where
    genVoteParams :: GeneratorT cep (ProposalKey -> Natural -> PermitProtected VoteParam)
    genVoteParams = do
      userPool <- get <&> gsAddresses
      from <- Gen.element userPool

      voteType <- Gen.bool

      let mkVoteParam proposalKey voteAmt = VoteParam
            { vProposalKey = proposalKey
            , vVoteType = voteType
            , vVoteAmount = voteAmt
            , vFrom = fst from
            }

      mkSigned <- sign' (snd from)

      mkMaybe <- Gen.element [\_ -> Nothing, \a -> Just a]

      let mkPermit signed =
            Permit (toPublic . snd $ from) (TSignature signed)

      pure $ \proposalKey correctVoteAmt ->
          let voteParam = mkVoteParam proposalKey correctVoteAmt
              ds = DataToSign
                  { dsChainId = dummyChainId
                  , dsContract =
                      ContractAddress $ unsafe $ parseContractHash "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"
                  , dsNonce = Nonce 0
                  , dsData = voteParam
                  }
              signed = mkSigned (lPackValueRaw ds)
          in PermitProtected voteParam (mkMaybe $ mkPermit signed)

-- | A version of `Tezos.Crypto.sign` to be run in `MonadGen`
sign' :: SecretKey -> GeneratorT cep ( ByteString -> Signature)
sign' addr = do
  seed <- drgNewSeed . seedFromInteger <$> Gen.integral (Range.linearFrom 0 -1000 1000)
  pure $ \bytes -> fst $ withDRG seed $ do
    sign addr bytes

type Parameter'' var = Parameter' (VariantToParam var)

genFlush :: GeneratorT var (Parameter'' var)
genFlush = do
  amt <- Gen.integral (Range.constant 0 100)
  pure $ XtzForbidden $ Flush amt

genDropProposal :: GeneratorT var (ProposalKey -> (Parameter'' var))
genDropProposal = do
  pure $ \key -> XtzForbidden $ Drop_proposal key

genUnstakeVote :: GeneratorT var (ProposalKey -> Parameter'' var)
genUnstakeVote = do
  pure $ \key -> XtzForbidden $ Unstake_vote [key]

genUpdateDelegate :: GeneratorT var (Parameter'' var)
genUpdateDelegate = do
  params <- genDelegateParams
  pure $ XtzForbidden $ Update_delegate params

genTransferContractTokens :: GeneratorT var (Address -> Parameter'' var)
genTransferContractTokens = do
  transfers <- genFa2TransferItems
  pure $ \govAddr -> XtzAllowed $ ConcreteEp $ Transfer_contract_tokens $ TransferContractTokensParam govAddr transfers

genTransferOwnership :: GeneratorT var (Parameter'' var)
genTransferOwnership = do
  userAddrs <- get <&> gsAddresses
  newOwner <- fst <$> Gen.element userAddrs
  pure $ XtzAllowed $ ConcreteEp $ Transfer_ownership $ (#newOwner :! newOwner)

-- Nothing to randomize, simply for consistency
genAcceptOwnership :: GeneratorT var (Parameter'' var)
genAcceptOwnership = do
  pure $ XtzAllowed $ ConcreteEp $ Accept_ownership ()

-------------------------------------------------------------
-- Shared
-------------------------------------------------------------

genXtzTransferType :: GeneratorT cep (Address -> TransferType)
genXtzTransferType = do
  amt <- Gen.integral (Range.constant 0 10)
  pure $ \contractAddr -> Xtz_transfer_type XtzTransfer
    { xtAmount = toMutez @Word63 amt
    , xtRecipient = contractAddr
    }

genTokenTransferType :: GeneratorT cep (Address -> TransferType)
genTokenTransferType = do
  transfers <- genFa2TransferItems

  pure $ \contractAddr -> Token_transfer_type TokenTransfer
    { ttContractAddress = contractAddr
    , ttTransferList = transfers
    }

genFa2TransferItem :: GeneratorT cep FA2.TransferItem
genFa2TransferItem = do
  amt <- Gen.integral (Range.constant 0 10)
  userAddrs <- get <&> gsAddresses
  fromAddr <- Gen.element userAddrs <&> fst
  toAddr <- Gen.element userAddrs <&> fst
  pure $ FA2.TransferItem
    { tiFrom = fromAddr
    , tiTxs = [ FA2.TransferDestination
        { tdTo = toAddr
        , tdTokenId = FA2.theTokenId
        , tdAmount = amt
        } ]
    }

genFa2TransferItems :: GeneratorT cep [FA2.TransferItem]
genFa2TransferItems =
  Gen.list (Range.linear 0 10) genFa2TransferItem

-------------------------------------------------------------
-- Helper
-------------------------------------------------------------

-- | Generate a list of items using given generator of a single item.
-- Takes a size parameter. Unlike QuickCheck's @vectorOf@, this function
-- can generate a smaller list (always non-empty).
vectorOf :: Int -> GeneratorT cep x -> GeneratorT cep [x]
vectorOf n = Gen.list (Range.linear 1 n)

-- Size of the random address pool
poolSize :: Int
poolSize = 2

fixedFee :: Natural
fixedFee = 10

getTotalSupply :: Map Address AddressFreezeHistory -> Natural
getTotalSupply freezeHistory =
  Map.elems freezeHistory
    & fmap (\AddressFreezeHistory{..} -> fhCurrentUnstaked + fhPastUnstaked + fhStaked)
    & sum

getStaked :: Map Address AddressFreezeHistory -> Natural
getStaked freezeHistory =
  Map.elems freezeHistory
    & fmap (\AddressFreezeHistory{..} -> fhStaked)
    & sum
