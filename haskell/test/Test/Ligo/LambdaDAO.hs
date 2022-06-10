-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.LambdaDAO
  ( test_LambdaDAO
  ) where

import Prelude

import Data.Set qualified as S
import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (assert, div)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Text (mkMText)
import Morley.Michelson.Typed (convertContract)
import Morley.Michelson.Typed.Convert (untypeValue)
import Morley.Tezos.Address
import Morley.Util.Named
import Test.Cleveland
import Test.Cleveland.Lorentz (contractConsumer)

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.RegistryDAO.Types
import Ligo.BaseDAO.LambdaDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.LambdaDAO.Types

getStorageRPCLambda :: forall p caps vd m. MonadCleveland caps m => TAddress p vd ->  m (StorageSkeletonRPC (VariantToExtra 'Lambda))
getStorageRPCLambda addr = getStorage @(StorageSkeleton (VariantToExtra 'Lambda)) (unTAddress addr)

withOriginated
  :: MonadCleveland caps m
  => Integer
  -> ([Address] -> LambdaStorage)
  -> ([Address] -> LambdaStorage -> TAddress (Parameter' LambdaCustomEpParam) () -> TAddress FA2.Parameter () -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ fromString ("address" <> (show x))) [1 ..addrCount]
  dodTokenContract <- chAddress <$> originateSimple "token_contract" [] dummyFA2Contract
  let storageInitial = storageFn addresses
  now_level <- getLevel
  let storage = storageInitial
        { sGovernanceToken = GovernanceToken
              { gtAddress = dodTokenContract
              , gtTokenId = FA2.theTokenId
              }
          , sStartLevel = now_level
        }

  baseDao <- originateUntyped $ UntypedOriginateData
    { uodName = "BaseDAO - LambdaDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal storage
    , uodContract = convertContract baseDAORegistryLigo
    }
  tests addresses storage (TAddress baseDao) (TAddress dodTokenContract)

toPeriod :: LambdaStorage -> Natural
toPeriod = unPeriod . cPeriod . sConfig

-- | We test non-token entrypoints of the BaseDAO contract here
test_LambdaDAO :: [TestTree]
test_LambdaDAO =
  [ testGroup "LambdaDAO Tests"
      [ testScenario "Calling the propose endpoint with an empty proposal works" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithExplictLambdaDAOConfig admin) $
           \(_:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Add_handler $ AddHandlerParam "proposal_1_handler" (undefined # undefined) undefined
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! proposalSize)
             startLevel <- getOriginationLevel' @'Registry baseDao

             -- Advance one voting period to a proposing stage.
             advanceToLevel (startLevel + toPeriod fs)

             withSender wallet1 $ call baseDao (Call @"Propose")
               (ProposeParams wallet1 proposalSize proposalMeta)
      ]
  ]
  where

  -- Here we parse the storage value from compiled ligo storage, which
  -- contains the RegistryDAO callbacks implemented in LIGO, and we just use
  -- `fromVal` to convert it to a 'Storage'. Then we can set the
  -- RegistryDAO configuration values using the setExtra function below, and
  -- initialize the contract using it. This let us have the callbacks from LIGO
  -- in storage, and allows to tweak RegistryDAO configuration in tests.
  initialStorage :: Address -> LambdaStorage
  initialStorage admin = let
    fs = baseDAORegistryStorageLigo { sExtra = def }
    in fs { sAdmin = admin, sConfig = (sConfig fs)
              { cPeriod = 11
              , cProposalFlushLevel = 22
              , cProposalExpiredLevel = 33
              , cGovernanceTotalSupply = 100

        }}

  initialStorageWithExplictLambdaDAOConfig :: Address -> LambdaStorage
  initialStorageWithExplictLambdaDAOConfig admin = (initialStorage admin)


--   [ testGroup "RegistryDAO Tests"
--     [ testScenario "Calling the propose endpoint with an empty proposal works" $ scenario $
--         withOriginated 2
--           (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin) $
--           \(_:wallet1:_) fs baseDao _ -> do
--             let proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                   Transfer_proposal $ TransferProposal 1 [] []
--             let proposalSize = metadataSize proposalMeta
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             startLevel <- getOriginationLevel' @'Registry baseDao
--
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + toPeriod fs)
--
--             withSender wallet1 $ call baseDao (Call @"Propose")
--               (ProposeParams wallet1 proposalSize proposalMeta)
--
--     , testScenario "proposal exceeding max_proposal_size result in error" $ scenario $
--         withOriginated 2
--           (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin) $
--           \(_:wallet1:_) _ baseDao _ -> let
--             -- In the explicitly set configuration max_proposal_size is set at 100.
--             -- And here we create a proposal that is bigger then 100.
--             proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--               Transfer_proposal $ TransferProposal 1 [] $
--                 [(unsafe $ mkMText ("long_key" <> (show @_ @Int t)), Just [mt|long_value|]) | t <- [1..10]]
--             proposalSize = metadataSize proposalMeta
--             in withSender wallet1 $ call
--                baseDao (Call @"Propose") (ProposeParams wallet1 proposalSize proposalMeta)
--                 & expectFailProposalCheck tooLargeProposalErrMsg
--
--     , testScenario "proposal_check: fail when xtz transfer contains 0 mutez" $ scenario $
--         withOriginated 2
--           (\(admin: _) ->
--               initialStorageWithExplictRegistryDAOConfig admin
--                 & setExtra (\re -> re { reMinXtzAmount = 0 })
--           ) $
--           \(_:wallet1:_) fs baseDao _ -> do
--             let proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                   Transfer_proposal $
--                     TransferProposal 1 [ xtzTransferType 0 wallet1 ] []
--             let proposalSize = metadataSize proposalMeta
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--
--             startLevel <- getOriginationLevel' @'Registry baseDao
--
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + toPeriod fs)
--
--             withSender wallet1 $ call baseDao (Call @"Propose")
--               (ProposeParams wallet1 proposalSize proposalMeta)
--                 & expectFailProposalCheck zeroMutezErrMsg
--
--     , testScenario "checks it fails if required tokens are not frozen" $ scenario $
--         withOriginated 2
--           (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin) $
--           \(_:wallet1:_) _ baseDao _ -> let
--             proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--               Transfer_proposal $ TransferProposal 1 [] []
--             -- Here we only freeze 2 tokens, but the proposal size and the configuration params
--             -- frozen_scale_value, frozen_extra_value set to 1 and 0 means that it requires 6
--             -- tokens to be frozen (6 * 1 + 0) because proposal size happen to be 6 here.
--             in withSender wallet1 $
--                call baseDao (Call @"Propose") (ProposeParams wallet1 2 proposalMeta)
--                & expectFailProposalCheck incorrectTokenAmountErrMsg
--
--     , testScenario "check it correctly calculates required frozen tokens" $ scenario $
--         withOriginated 2
--           (\(admin: _) -> setExtra (\re -> re { reFrozenExtraValue = 2 })  $ initialStorageWithExplictRegistryDAOConfig admin) $
--           \(_:wallet1:_) fs baseDao _ -> do
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Transfer_proposal $ TransferProposal 1 [] []
--               proposalSize = metadataSize proposalMeta -- 10
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 2))
--
--             startLevel <- getOriginationLevel' @'Registry baseDao
--
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + toPeriod fs)
--
--             -- Here the proposal size and the configuration params frozen_scale_value,
--             -- frozen_extra_value set to 1 and 2 means that it requires 12 tokens to be
--             -- frozen (10 * 1 + 2) if proposal size is 10.
--             withSender wallet1 $
--                call baseDao (Call @"Propose") (ProposeParams wallet1 (proposalSize + 2) proposalMeta)
--
--     , testScenario "checks it correctly calculates tokens to burn when rejecting" $ scenario $ do
--         let frozen_scale_value = 2
--         let frozen_extra_value = 0
--         let slash_scale_value = 1
--         let slash_division_value = 4
--         withOriginated 2
--           (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin &
--             setExtra (\re -> re { reFrozenScaleValue = frozen_scale_value }) & -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
--             setExtra (\re -> re { reFrozenExtraValue = frozen_extra_value }) & -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
--             setExtra (\re -> re { reSlashScaleValue = slash_scale_value }) & -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
--             setExtra (\re -> re { reSlashDivisionValue = slash_division_value }) -- @Natural [mt|frozen_scale_value|] frozen_scale_value $
--             ) $
--
--           \(admin: wallet1: _) (toPeriod -> period) baseDao _ -> let
--             proposalMeta1 = lPackValueRaw @RegistryDaoProposalMetadata $
--               Transfer_proposal $ TransferProposal 1 [] []
--             proposalSize1 = metadataSize proposalMeta1
--
--             in do
--               let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value
--
--               withSender wallet1 $
--                 call baseDao (Call @"Freeze") (#amount :! requiredFrozen)
--
--               startLevel <- getOriginationLevel' @'Registry baseDao
--
--               -- Advance one voting period to a proposing stage.
--               advanceToLevel (startLevel + period)
--
--               let params = ProposeParams wallet1 requiredFrozen proposalMeta1
--               let proposalKey = makeProposalKey params
--               withSender wallet1 $
--                 call baseDao (Call @"Propose") params
--
--               -- Advance one voting period to a proposing stage.
--               proposalStart <- getProposalStartLevel' @'Registry baseDao proposalKey
--               advanceToLevel (proposalStart + 2*period + 1)
--               withSender admin $
--                 call baseDao (Call @"Flush") (1 :: Natural)
--
--               -- Since we have frozen_scale_value = 2, slash_scale_value = 1 and slash_division_value = 2
--               -- After the rejection above, we expect that (2 * proposalSize1/2) tokens
--               -- will be returned to the proposer.  So they will have a balance
--               -- of 100 - proposalSize1.
--
--               let spent = div (requiredFrozen * slash_scale_value) slash_division_value
--
--               checkBalance' @'Registry baseDao wallet1 (requiredFrozen - spent)
--
--
--     , testScenario "checks it correctly executes the proposal that has won" $ scenario $ do
--         let frozen_scale_value = 1
--         let frozen_extra_value = 0
--         let slash_scale_value = 1
--         let slash_division_value = 1
--         withOriginated 3
--           (\(admin: _) ->
--             initialStorageWithExplictRegistryDAOConfig admin &
--             setExtra (\re -> re { reFrozenScaleValue = frozen_scale_value }) &
--             setExtra (\re -> re { reFrozenExtraValue = frozen_extra_value }) &
--             setExtra (\re -> re { reSlashScaleValue = slash_scale_value }) &
--             setExtra (\re -> re { reMaxProposalSize = 200 }) &
--             setExtra (\re -> re { reSlashDivisionValue = slash_division_value })) $
--
--           \(admin: wallet1: voter1 : _) (toPeriod -> period) baseDao _ -> let
--
--             in do
--               withSender wallet1 $
--                 call baseDao (Call @"Freeze") (#amount :! 400)
--
--               withSender voter1 $
--                 call baseDao (Call @"Freeze") (#amount :! 100)
--
--               startLevel <- getOriginationLevel' @'Registry baseDao
--               -- Advance one voting period to a proposing stage.
--               advanceToLevel (startLevel + period)
--
--               let sMaxUpdateproposalMeta1 = lPackValueRaw @RegistryDaoProposalMetadata $
--                     Configuration_proposal $ ConfigProposal
--                         { cpFrozenScaleValue = Nothing
--                         , cpFrozenExtraValue = Nothing
--                         , cpSlashScaleValue = Nothing
--                         , cpSlashDivisionValue = Nothing
--                         , cpMaxProposalSize = Just 341
--                         }
--               let sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
--               let requiredFrozenForUpdate = sMaxUpdateproposalSize1 * frozen_scale_value + frozen_extra_value
--
--               withSender wallet1 $
--                 call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozenForUpdate sMaxUpdateproposalMeta1)
--
--               -- Advance one voting period to a voting stage.
--               advanceToLevel (startLevel + 2* period)
--               -- Then we send 60 upvotes for the proposal (as min quorum is 1% of 500)
--               let proposalKey = makeProposalKey (ProposeParams wallet1 requiredFrozenForUpdate sMaxUpdateproposalMeta1)
--               withSender voter1 $
--                 call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 60 voter1) Nothing]
--
--               proposalStart <- getProposalStartLevel' @'Registry baseDao proposalKey
--               advanceToLevel (proposalStart + 2*period)
--               withSender admin $
--                 call baseDao (Call @"Flush") (1 :: Natural)
--
--               maxProposalSize <- (reMaxProposalSizeRPC . sExtraRPC) <$> getStorageRPCRegistry baseDao
--               assert (maxProposalSize == 341) "Unexpected max_proposal_size update"
--
--     , testScenario "checks on-chain view correctly returns the registry value" $ scenario $ do
--         -- The default values assigned from initialStorageWithExplictRegistryDAOConfig function
--         withOriginated 3
--           (\(admin:_) -> setExtra (\re -> re { reMaxProposalSize = 200 }) $ initialStorageWithExplictRegistryDAOConfig admin) $
--
--           \(admin: wallet1: voter1 : _) (toPeriod -> period) baseDao _ -> do
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Transfer_proposal $ TransferProposal 1 [] [([mt|key|], Just [mt|testVal|])]
--
--               proposalSize = metadataSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--
--             withSender voter1 $
--               call baseDao (Call @"Freeze") (#amount :! 50)
--             -- Advance one voting period to a proposing stage.
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             advanceToLevel (startLevel + period)
--
--             let requiredFrozen = proposalSize -- since frozen_scale_value and frozen_scale_value are 1 and 0.
--
--             -- We propose the addition of a new registry key
--             withSender wallet1 $
--               call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozen proposalMeta)
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
--             let proposalKey = makeProposalKey (ProposeParams wallet1 requiredFrozen proposalMeta)
--             withSender voter1 $
--               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 50 voter1) Nothing]
--
--             -- Advance one voting period to a proposing stage.
--             proposalStart <- getProposalStartLevel' @'Registry baseDao proposalKey
--             advanceToLevel (proposalStart + 2*period)
--             withSender admin $
--               call baseDao (Call @"Flush") (1 :: Natural)
--
--             consumer <- chAddress <$> originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))
--
--             withSender voter1 $
--               call baseDao (Call @"Lookup_registry") (LookupRegistryParam [mt|key|] consumer)
--
--             checkStorage @[(MText, (Maybe MText))] consumer ([([mt|key|], Just [mt|testVal|])])
--
--     , testScenario "checks it can flush a transfer type proposal (#66)" $ scenario $
--         withOriginated 3
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) (initialStorageWithExplictRegistryDAOConfig admin)) $
--           \[admin, wallet1, wallet2] (toPeriod -> period) baseDao dodTokenContract -> do
--
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Transfer_proposal $ TransferProposal
--                   1
--                   [ tokenTransferType (toAddress dodTokenContract) wallet2 wallet1]
--                   []
--               proposalSize = metadataSize proposalMeta
--               proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             withSender wallet2 $
--               call baseDao (Call @"Freeze") (#amount :! 20)
--
--             -- Advance one voting period to a proposing stage.
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             advanceToLevel (startLevel + period)
--
--             withSender wallet1 $
--               call baseDao (Call @"Propose") proposeParams
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--
--             let
--               key1 = makeProposalKey proposeParams
--               upvote = NoPermit VoteParam
--                   { vFrom = wallet2
--                   , vVoteType = True
--                   , vVoteAmount = 20
--                   , vProposalKey = key1
--                   }
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
--             -- Advance one voting period to a proposing stage.
--             proposalStart <- getProposalStartLevel' @'Registry baseDao key1
--             advanceToLevel (proposalStart + 2*period)
--             withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--             checkBalance' @'Registry baseDao wallet2 20
--
--             checkStorage (unTAddress dodTokenContract)
--               ( [ [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
--                 , [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 20 }] } ] -- Wallet2 freezes 20 tokens
--                 , [ FA2.TransferItem { tiFrom = wallet1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
--                 ])
--
--     , testScenario "checks it can propose a valid xtz type proposal (#66)" $ scenario $
--         withOriginated 2
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[_, wallet] (toPeriod -> period) baseDao _ -> do
--             let
--               proposalMeta amt = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Transfer_proposal $
--                   TransferProposal 1 [ xtzTransferType amt wallet ] []
--               proposeParams amt = ProposeParams wallet (metadataSize $ proposalMeta amt) $ proposalMeta amt
--               proposalSize amt = metadataSize $ proposalMeta amt
--
--             withSender wallet $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize 10)
--
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + period)
--
--             -- Fails because 10 >= max_xtz_amount
--             withSender wallet $
--               call baseDao (Call @"Propose") (proposeParams 10)
--               & expectFailProposalCheck tooLargeXtzErrMsg
--
--             -- Fails because 1 >= min_xtz_amount
--             withSender wallet $
--               call baseDao (Call @"Propose") (proposeParams 1)
--               & expectFailProposalCheck tooSmallXtzErrMsg
--
--             withSender wallet $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize 3)
--
--             -- Advance two voting period to another proposing stage.
--             advanceToLevel (startLevel + 3*period)
--
--             withSender wallet $
--               call baseDao (Call @"Propose") (proposeParams 3)
--
--             checkBalance' @'Registry baseDao wallet (proposalSize 3 + proposalSize 10)
--
--
--     , testScenario "can flush a transfer proposal with registry updates" $ scenario $
--         withOriginated 3
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[admin, wallet1, wallet2] (toPeriod -> period) baseDao dodTokenContract -> do
--
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Transfer_proposal $ TransferProposal
--                   1
--                   [ tokenTransferType (toAddress dodTokenContract) wallet2 wallet1 ]
--                   [ ([mt|testKey|], Just [mt|testValue|]) ]
--               proposalSize = metadataSize proposalMeta
--               proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             withSender wallet2 $
--               call baseDao (Call @"Freeze") (#amount :! 50)
--
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + period)
--
--             -- We propose the addition of a new registry key *and* a token transfer
--             withSender wallet1 $
--               call baseDao (Call @"Propose") proposeParams
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
--             let proposalKey = makeProposalKey (ProposeParams wallet1 proposalSize proposalMeta)
--             withSender wallet2 $
--               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 50 wallet2) Nothing]
--
--             proposalStart <- getProposalStartLevel' @'Registry baseDao proposalKey
--             advanceToLevel (proposalStart + 2*period)
--             withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--
--             -- check the registry update
--             consumer <- chAddress <$> originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))
--             withSender wallet2 $ call baseDao (Call @"Lookup_registry") (LookupRegistryParam [mt|testKey|] consumer)
--             checkStorage consumer ([([mt|testKey|], Just [mt|testValue|])])
--
--             -- check the balance
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--             checkBalance' @'Registry baseDao wallet2 50
--
--             checkStorage (unTAddress dodTokenContract)
--               [ [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
--                 , [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 50 }] } ] -- Wallet2 freezes 50 tokens
--                 , [ FA2.TransferItem { tiFrom = wallet1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
--               ]
--
--     , testScenario "checks it can flush a transfer proposal" $ scenario $
--         withOriginated 3
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[admin, wallet1, wallet2] (toPeriod -> period) baseDao _ -> do
--
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Transfer_proposal $
--                   TransferProposal 1 [ xtzTransferType 3 wallet2 ] []
--               proposalSize = metadataSize proposalMeta
--               proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             withSender wallet2 $
--               call baseDao (Call @"Freeze") (#amount :! 10)
--             sendXtz baseDao
--             startLevel <- getOriginationLevel' @'Registry baseDao
--
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + period)
--
--             withSender wallet1 $
--               call baseDao (Call @"Propose") proposeParams
--             let key1 = makeProposalKey proposeParams
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--
--
--             let
--               upvote = NoPermit VoteParam
--                 { vFrom = wallet2
--                 , vVoteType = True
--                 , vVoteAmount = 2
--                 , vProposalKey = key1
--                 }
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
--             proposalStart <- getProposalStartLevel' @'Registry baseDao key1
--             advanceToLevel (proposalStart + 2*period + 1)
--             withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--
--     , testScenario "checks it can flush a proposal that updates guardian address" $ scenario $
--         withOriginated 4
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[admin, wallet1, wallet2, newGuardian] (toPeriod -> period) baseDao _ -> do
--
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Update_guardian newGuardian
--
--               proposalSize = metadataSize proposalMeta
--               proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             withSender wallet2 $
--               call baseDao (Call @"Freeze") (#amount :! 20)
--
--             -- Advance one voting period to a proposing stage.
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             advanceToLevel (startLevel + period)
--
--             withSender wallet1 $
--               call baseDao (Call @"Propose") proposeParams
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--
--             let
--               key1 = makeProposalKey proposeParams
--               upvote = NoPermit VoteParam
--                   { vFrom = wallet2
--                   , vVoteType = True
--                   , vVoteAmount = 20
--                   , vProposalKey = key1
--                   }
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
--             proposalStart <- getProposalStartLevel' @'Registry baseDao key1
--             advanceToLevel (proposalStart + 2*period)
--             withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--
--             checkGuardian' @'Registry baseDao newGuardian
--
--     , testScenario "checks it can flush a proposal that updates contract delegate address" $ scenario $
--         withOriginated 4
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[admin, wallet1, wallet2, delegateAddr] (toPeriod -> period) baseDao _ -> do
--             registerDelegate delegateAddr
--             case delegateAddr of
--               KeyAddress delegate -> do
--                 let
--                   proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                     Update_contract_delegate $ Just delegate
--
--                   proposalSize = metadataSize proposalMeta
--                   proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--                 withSender wallet1 $
--                   call baseDao (Call @"Freeze") (#amount :! proposalSize)
--                 withSender wallet2 $
--                   call baseDao (Call @"Freeze") (#amount :! 20)
--
--                 -- Advance one voting period to a proposing stage.
--                 startLevel <- getOriginationLevel' @'Registry baseDao
--                 advanceToLevel (startLevel + period)
--
--                 withSender wallet1 $
--                   call baseDao (Call @"Propose") proposeParams
--
--                 checkBalance' @'Registry baseDao wallet1 proposalSize
--
--                 let
--                   key1 = makeProposalKey proposeParams
--                   upvote = NoPermit VoteParam
--                       { vFrom = wallet2
--                       , vVoteType = True
--                       , vVoteAmount = 20
--                       , vProposalKey = key1
--                       }
--
--                 -- Advance one voting period to a voting stage.
--                 advanceToLevel (startLevel + 2*period)
--                 withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
--                 proposalStart <- getProposalStartLevel' @'Registry baseDao key1
--                 advanceToLevel (proposalStart + 2*period)
--                 withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--                 getDelegate baseDao @@== (Just delegate)
--               _ -> error "impossible"
--
--     , testScenario "checks it can flush an Update_receivers_proposal" $ scenario $
--         withOriginated 3
--           (\(admin : _) ->
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[admin, wallet1, wallet2] (toPeriod -> period) baseDao _ -> do
--
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Update_receivers_proposal $ Add_receivers [wallet1, wallet2]
--
--               proposalSize = metadataSize proposalMeta
--               proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             withSender wallet2 $
--               call baseDao (Call @"Freeze") (#amount :! 20)
--
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             -- Advance one voting period to a proposing stage.
--             advanceToLevel (startLevel + period)
--
--             withSender wallet1 $
--               call baseDao (Call @"Propose") proposeParams
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--
--             let
--               key1 = makeProposalKey proposeParams
--               upvote = NoPermit VoteParam
--                   { vFrom = wallet2
--                   , vVoteType = True
--                   , vVoteAmount = 20
--                   , vProposalKey = key1
--                   }
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
--             -- Advance one voting period to a proposing stage.
--             proposalStart <- getProposalStartLevel' @'Registry baseDao key1
--             advanceToLevel (proposalStart + 2*period)
--             withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--
--             receiversSet <- (reProposalReceiversRPC . sExtraRPC) <$> getStorageRPCRegistry baseDao
--             assert ((wallet1 `S.member` receiversSet) && (wallet2 `S.member` receiversSet))
--               "Proposal receivers was not updated as expected"
--
--     , testScenario "checks it can flush an Update_receivers_proposal that deletes" $ scenario $
--         withOriginated 3
--           (\(admin : wallet1 : wallet2 : _) ->
--             setExtra (\re -> re { reProposalReceivers = (S.fromList [wallet1, wallet2]) }) $
--             setExtra (\re -> re { reMaxProposalSize = 200 }) $
--             initialStorageWithExplictRegistryDAOConfig admin) $
--           \[admin, wallet1, wallet2] (toPeriod -> period) baseDao _ -> do
--
--             let
--               proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
--                 Update_receivers_proposal $ Remove_receivers [wallet1]
--
--               proposalSize = metadataSize proposalMeta
--               proposeParams = ProposeParams wallet1 proposalSize proposalMeta
--
--             withSender wallet1 $
--               call baseDao (Call @"Freeze") (#amount :! proposalSize)
--             withSender wallet2 $
--               call baseDao (Call @"Freeze") (#amount :! 20)
--
--             -- Advance one voting period to a proposing stage.
--             startLevel <- getOriginationLevel' @'Registry baseDao
--             advanceToLevel (startLevel + period)
--
--             withSender wallet1 $
--               call baseDao (Call @"Propose") proposeParams
--
--             checkBalance' @'Registry baseDao wallet1 proposalSize
--
--             let
--               key1 = makeProposalKey proposeParams
--               upvote = NoPermit VoteParam
--                   { vFrom = wallet2
--                   , vVoteType = True
--                   , vVoteAmount = 20
--                   , vProposalKey = key1
--                   }
--
--             -- Advance one voting period to a voting stage.
--             advanceToLevel (startLevel + 2*period)
--             withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
--             -- Advance one voting period to a proposing stage.
--             proposalStart <- getProposalStartLevel' @'Registry baseDao key1
--             advanceToLevel (proposalStart + 2*period)
--             withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)
--
--             receiversSet <- (reProposalReceiversRPC . sExtraRPC) <$> getStorageRPCRegistry baseDao
--             assert (Prelude.not (wallet1 `S.member` receiversSet))
--               "Proposal receivers was not updated as expected"
--     ]
--   ]
--   where
--
--     -- Here we parse the storage value from compiled ligo storage, which
--     -- contains the RegistryDAO callbacks implemented in LIGO, and we just use
--     -- `fromVal` to convert it to a 'Storage'. Then we can set the
--     -- RegistryDAO configuration values using the setExtra function below, and
--     -- initialize the contract using it. This let us have the callbacks from LIGO
--     -- in storage, and allows to tweak RegistryDAO configuration in tests.
--     initialStorage :: Address -> RegistryStorage
--     initialStorage admin = let
--       fs = baseDAORegistryStorageLigo { sExtra = def }
--       in fs { sAdmin = admin, sConfig = (sConfig fs)
--                 { cPeriod = 11
--                 , cProposalFlushLevel = 22
--                 , cProposalExpiredLevel = 33
--                 , cGovernanceTotalSupply = 100
--
--           }}
--
--     initialStorageWithExplictRegistryDAOConfig :: Address -> RegistryStorage
--     initialStorageWithExplictRegistryDAOConfig admin = (initialStorage admin)
--       & setExtra (\re -> re { reRegistry = mempty })
--       & setExtra (\re -> re { reRegistryAffected = mempty })
--       & setExtra (\re -> re { reProposalReceivers = S.empty })
--       & setExtra (\re -> re { reFrozenScaleValue = 1 })
--       & setExtra (\re -> re { reFrozenExtraValue = 0 })
--       & setExtra (\re -> re { reSlashScaleValue = 1 })
--       & setExtra (\re -> re { reSlashDivisionValue = 1 })
--       & setExtra (\re -> re { reMinXtzAmount = 2 })
--       & setExtra (\re -> re { reMaxXtzAmount = 5 })
--       & setExtra (\re -> re { reMaxProposalSize = 100 })

expectFailProposalCheck
  :: (MonadCleveland caps m)
  => MText -> m a -> m ()
expectFailProposalCheck err =
  expectFailedWith (failProposalCheck, err)

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
      , tiTxs =
        [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = FA2.theTokenId
          , tdAmount = 10
          }
        ]
      }
    ]
  }
