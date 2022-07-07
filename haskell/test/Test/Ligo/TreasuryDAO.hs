{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}
-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.TreasuryDAO
  ( test_TreasuryDAO
  , treasuryDAOTests
  ) where

import Universum

import Lorentz
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Tezos.Address
import Morley.Util.Named
import Test.Cleveland
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Contract (baseDAOTreasuryLigo, baseDAOTreasuryStorageLigo)
import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.TreasuryDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.TreasuryDAO.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

instance IsProposalArgument 'Treasury (Maybe KeyHash) where
  toMetadata a = lPackValueRaw @TreasuryDaoProposalMetadata $ Update_contract_delegate a

instance IsProposalArgument 'Treasury TransferProposal where
  toMetadata a = lPackValueRaw @TreasuryDaoProposalMetadata $ Transfer_proposal a

instance IsProposalArgument 'Treasury Address where
  toMetadata a = lPackValueRaw @TreasuryDaoProposalMetadata $ Update_guardian a

instance TestableVariant 'Treasury where
  getInitialStorage admin = initialStorageWithExplictTreasuryDAOConfig admin
  getContract = baseDAOTreasuryLigo
  getVariantStorageRPC addr = getStorage @(StorageSkeleton (VariantToExtra 'Treasury)) (unTAddress addr)

instance VariantExtraHasField 'Treasury "MinXtzAmount" Mutez where
  setVariantExtra v = setExtra (\re -> re { teMinXtzAmount = v })
  getVariantExtra = teMinXtzAmount

instance VariantExtraHasField 'Treasury "MaxProposalSize" Natural where
  setVariantExtra v = setExtra (\re -> re { teMaxProposalSize = v })
  getVariantExtra = teMaxProposalSize

instance VariantExtraHasField 'Treasury "FrozenExtraValue" Natural where
  setVariantExtra v = setExtra (\re -> re { teFrozenExtraValue = v })
  getVariantExtra = teFrozenExtraValue

instance VariantExtraHasField 'Treasury "FrozenScaleValue" Natural where
  setVariantExtra v = setExtra (\re -> re { teFrozenScaleValue = v })
  getVariantExtra = teFrozenScaleValue

instance VariantExtraHasField 'Treasury "SlashScaleValue" Natural where
  setVariantExtra v = setExtra (\re -> re { teSlashScaleValue = v })
  getVariantExtra = teSlashScaleValue

instance VariantExtraHasField 'Treasury "SlashDivisionValue" Natural where
  setVariantExtra v = setExtra (\re -> re { teSlashDivisionValue = v })
  getVariantExtra = teSlashDivisionValue


-- | Testing a Treasury-like DAO. Ex. DNS Treasury
test_TreasuryDAO :: TestTree
test_TreasuryDAO = treasuryDAOTests @'Treasury

type TreasuryConstraints variant =
  ( IsProposalArgument variant TransferProposal
  , IsProposalArgument variant Address
  , IsProposalArgument variant (Maybe KeyHash)
  , TestableVariant variant
  , VariantExtraHasField variant "MinXtzAmount" Mutez
  , VariantExtraHasField variant "MaxProposalSize" Natural
  , VariantExtraHasField variant "FrozenExtraValue" Natural
  , VariantExtraHasField variant "FrozenScaleValue" Natural
  , VariantExtraHasField variant "SlashScaleValue" Natural
  , VariantExtraHasField variant "SlashDivisionValue" Natural
  )

treasuryDAOTests
  :: forall variant. TreasuryConstraints variant  => TestTree
treasuryDAOTests = testGroup "TreasuryDAO Tests"
  [ testGroup "Proposal creator:"
      [ testScenario "can propose a valid proposal" $ scenario $
          validProposal @variant
      , testScenario "can flush a Token transfer proposal" $ scenario $
          flushTokenTransfer @variant
      , testScenario "can flush a Xtz transfer proposal" $ scenario $
          flushXtzTransfer @variant
      , testScenario "can flush a Update_guardian proposal" $ scenario $
          flushUpdateGuardian @variant
      , testScenario "can flush a Update_contract_delegate proposal" $ scenario $
          flushUpdateContractDelegate @variant
      ]

  , testGroup "proposal_check:"
      [ testScenario "fail when xtz transfer contains 0 mutez" $ scenario $
          proposalCheckFailZeroMutez @variant
      , testScenario "fail when proposal size is bigger than max" $ scenario $
          proposalCheckBiggerThanMaxProposalSize @variant
      ]
  ]

validProposal
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
validProposal = withFrozenCallStack $ withOriginated @variant 3
  (\_ s -> s { sConfig = (sConfig s) { cPeriod = 20 } }) $
  \(_: dodOwner1: dodOwner2 : _) fs dodDao _ -> do
    let dodPeriod = toPeriod fs
    startLevel <- getOriginationLevel' @variant dodDao
    let
      proposalMeta = toProposalMetadata @variant $ TransferProposal
          { tpAgoraPostId = 1
          , tpTransfers = [ tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 ]
          }
      proposalSize = metadataSize proposalMeta -- 115

    -- Freeze in voting stage.
    withSender dodOwner1 $
      call dodDao (Call @"Freeze") (#amount :! proposalSize)

    -- Advance one voting period to a proposing stage.
    advanceToLevel (startLevel + dodPeriod + 1)

    withSender dodOwner1 $
      call dodDao (Call @"Propose") (ProposeParams dodOwner1 (proposalSize + 1) proposalMeta)
      & expectFailedWith (failProposalCheck, incorrectTokenAmountErrMsg)

    withSender dodOwner1 $
      call dodDao (Call @"Propose") (ProposeParams dodOwner1 proposalSize proposalMeta)

    checkBalance' @variant dodDao dodOwner1 (proposalSize)

flushTokenTransfer
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
flushTokenTransfer = withFrozenCallStack $ withOriginated @variant 3
  (\_ s -> s { sConfig = (sConfig s) { cPeriod = 20, cProposalExpiredLevel = 300 } }) $
  \(dodAdmin : dodOwner1: dodOwner2 : _) fs dodDao dodTokenContract -> do
  let dodPeriod = toPeriod fs
  startLevel <- getOriginationLevel' @variant dodDao

  let
    proposalMeta = toProposalMetadata @variant $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodTokenContract) dodOwner2 dodOwner1 ]
        }
    proposalSize = metadataSize proposalMeta
    proposeParams = ProposeParams dodOwner1 proposalSize proposalMeta

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! proposalSize)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 20)

  -- Advance one voting periods to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $ call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams

  checkBalance' @variant dodDao dodOwner1 proposalSize

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel' @variant dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  checkBalance' @variant dodDao dodOwner1 proposalSize
  checkBalance' @variant dodDao dodOwner2 20

flushXtzTransfer
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
flushXtzTransfer = withFrozenCallStack $ withOriginated @variant 3
  (\_ s -> s { sConfig = (sConfig s) { cPeriod = 25, cProposalExpiredLevel = 300 } }) $
  \(dodAdmin : dodOwner1: dodOwner2 : _) fs dodDao _ -> do
  let dodPeriod = toPeriod fs
  originationLevel <- getOriginationLevel' @variant dodDao

  let
    proposalMeta amt = toProposalMetadata @variant $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType amt dodOwner2 ]
        }
    proposeParams amt = ProposeParams dodOwner1 (metadataSize $ proposalMeta amt) $ proposalMeta amt

  let mdSize = metadataSize $ proposalMeta 3
  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! mdSize)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 20)
  -- Advance one voting period to a proposing stage.
  sendXtz (TAddress $ toAddress dodDao)
  advanceToLevel (originationLevel + dodPeriod)

  withSender dodOwner1 $ do
    -- due to smaller than min_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 1)
      & expectFailedWith (failProposalCheck, tooSmallXtzErrMsg)

    -- due to bigger than max_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 6)
      & expectFailedWith (failProposalCheck, tooLargeXtzErrMsg)

    call dodDao (Call @"Propose") (proposeParams 3)
  let key1 = makeProposalKey (proposeParams 3)

  checkBalance' @variant dodDao dodOwner1 mdSize

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (originationLevel + 2*dodPeriod + 1)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel' @variant dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  --TODO: check xtz balance

flushUpdateGuardian
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
flushUpdateGuardian = withFrozenCallStack $ withOriginated @variant 3
  (\_ s -> s { sConfig = (sConfig s) { cPeriod = 25, cProposalExpiredLevel = 300 } }) $
  \(dodAdmin : dodOwner1: dodOwner2 : _) fs dodDao _ -> do
  let dodPeriod = toPeriod fs

  let
    proposalMeta = toProposalMetadata @variant dodOwner2
    proposeParams = ProposeParams dodOwner1 (metadataSize $ proposalMeta) $ proposalMeta

  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! (metadataSize $ proposalMeta))

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount :! 20)
  sendXtz (TAddress $ toAddress dodDao)
  -- Advance one voting period to a proposing stage.
  startLevel <- getOriginationLevel' @variant dodDao
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  proposalStart <- getProposalStartLevel' @variant dodDao key1
  advanceToLevel (proposalStart + 2*dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  checkGuardian' @variant dodDao dodOwner2

flushUpdateContractDelegate
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
flushUpdateContractDelegate = withFrozenCallStack $ withOriginated @variant 4
  (\_ s -> s { sConfig = (sConfig s) { cPeriod = 25, cProposalExpiredLevel = 300 } }) $
  \(dodAdmin : dodOwner1: dodOwner2 : dodOperator2 : _) fs dodDao _ -> do
  let dodPeriod = toPeriod fs
  registerDelegate dodOperator2
  case dodOperator2 of
    KeyAddress delegate -> do
      let
        proposalMeta = toProposalMetadata @variant $ Just delegate
        proposeParams = ProposeParams dodOwner1 (metadataSize $ proposalMeta) $ proposalMeta

      -- Freeze in initial voting stage.
      withSender dodOwner1 $
        call dodDao (Call @"Freeze") (#amount :! (metadataSize $ proposalMeta))

      withSender dodOwner2 $
        call dodDao (Call @"Freeze") (#amount :! 20)
      sendXtz (TAddress $ toAddress dodDao)
      -- Advance one voting period to a proposing stage.
      startLevel <- getOriginationLevel' @variant dodDao
      advanceToLevel (startLevel + dodPeriod)

      withSender dodOwner1 $
        call dodDao (Call @"Propose") proposeParams
      let key1 = makeProposalKey proposeParams

      let
        upvote = NoPermit VoteParam
            { vFrom = dodOwner2
            , vVoteType = True
            , vVoteAmount = 20
            , vProposalKey = key1
            }

      -- Advance one voting period to a voting stage.
      advanceToLevel (startLevel + 2*dodPeriod)
      withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
      -- Advance one voting period to a proposing stage.
      proposalStart <- getProposalStartLevel' @variant dodDao key1
      advanceToLevel (proposalStart + 2*dodPeriod + 1)
      withSender dodAdmin $ call dodDao (Call @"Flush") 100
      getDelegate dodDao @@== (Just delegate)
    _ -> error "impossible"

proposalCheckFailZeroMutez
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
proposalCheckFailZeroMutez = withFrozenCallStack $ withOriginated @variant 3
  (\_ s -> setVariantExtra @variant @"MinXtzAmount" zeroMutez s) $
  \(_ : dodOwner1: dodOwner2 : _) fs dodDao _ -> do
  let dodPeriod = toPeriod fs

  startLevel <- getOriginationLevel' @variant dodDao

  let
    proposalMeta = toProposalMetadata @variant $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType 0 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 proposalSize proposalMeta)
      & expectFailedWith (failProposalCheck, zeroMutezErrMsg)

proposalCheckBiggerThanMaxProposalSize
  :: forall variant caps m. (TreasuryConstraints variant, MonadCleveland caps m, HasCallStack)
  => m ()
proposalCheckBiggerThanMaxProposalSize = withFrozenCallStack $ withOriginated @variant 3
  (\_ s ->  s) $
  \(_ : dodOwner1: dodOwner2 : _) fs dodDao _ -> do
  let dodPeriod = toPeriod fs
  startLevel <- getOriginationLevel' @variant dodDao
  let
    largeProposalMeta = toProposalMetadata @variant $ TransferProposal 1 $
        [tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 | (_ :: Integer) <- [1..10]]
    largeProposalSize = metadataSize largeProposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount :! largeProposalSize)

  -- Advance one voting period to a proposing stage.
  advanceToLevel (startLevel + dodPeriod)

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 largeProposalSize largeProposalMeta)
      & expectFailedWith (failProposalCheck, tooLargeProposalErrMsg)


--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

xtzTransferType :: Word32 -> Address -> TransferType
xtzTransferType amt toAddr = Xtz_transfer_type XtzTransfer
  { xtAmount = toMutez amt
  , xtRecipient = toAddr
  }

tokenTransferType :: Address -> Address -> Address -> TransferType
tokenTransferType contractAddr fromAddr toAddr = Token_transfer_type TokenTransfer
  { ttContractAddress = contractAddr
  , ttTransferList =
      [ FA2.TransferItem
      { tiFrom = fromAddr
      , tiTxs = [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = FA2.theTokenId
          , tdAmount = 10
          } ]
      } ]
  }

-- Here we parse the storage value from compiled ligo storage, which
-- contains the RegistryDAO callbacks implemented in LIGO, and we just use
-- `fromVal` to convert it to a 'Storage'. Then we can set the
-- RegistryDAO configuration values using the setExtra function below, and
-- initialize the contract using it. This let us have the callbacks from LIGO
-- in storage, and allows to tweak RegistryDAO configuration in tests.
initialStorage :: Address -> TreasuryStorage
initialStorage admin = let
  fs = baseDAOTreasuryStorageLigo
  in fs { sAdmin = admin, sConfig = (sConfig fs)
            { cPeriod = 10
            , cProposalFlushLevel = 20
            , cProposalExpiredLevel = 30
            , cGovernanceTotalSupply = 100
            , cMinQuorumThreshold = fromIntegral $ mkQuorumThreshold 1 100

      }}

initialStorageWithExplictTreasuryDAOConfig :: Address -> TreasuryStorage
initialStorageWithExplictTreasuryDAOConfig admin = (initialStorage admin)
  & setExtra (\te -> te { teFrozenScaleValue = 1 })
  & setExtra (\te -> te { teFrozenExtraValue = 0 })
  & setExtra (\te -> te { teSlashScaleValue = 1 })
  & setExtra (\te -> te { teSlashDivisionValue = 1 })
  & setExtra (\te -> te { teMinXtzAmount = 2 })
  & setExtra (\te -> te { teMaxXtzAmount = 5 })
  & setExtra (\te -> te { teMaxProposalSize = 1000 })
