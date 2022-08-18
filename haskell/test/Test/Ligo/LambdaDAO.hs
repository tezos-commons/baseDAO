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

import Data.Map qualified as M
import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (assert, div)
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Typed (convertContract)
import Morley.Michelson.Typed.Convert (untypeValue)
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.LambdaDAO.Types
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

getStorageRPCLambda :: forall p caps vd m. MonadCleveland caps m => TAddress p vd ->  m (StorageSkeletonRPC (VariantToExtra 'Lambda))
getStorageRPCLambda addr = getStorage @(StorageSkeleton (VariantToExtra 'Lambda)) (unTAddress addr)

withOriginated
  :: MonadCleveland caps m
  => Integer
  -> ([Address] -> LambdaStorage)
  -> ([Address] -> LambdaStorage -> TAddress (Parameter' LambdaCustomEpParam) () -> TAddress FA2.Parameter () -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> refillable $ newAddress $ fromString ("address" <> (show x))) [1 ..addrCount]
  dodTokenContract <- chAddress <$> originateSimple "token_contract" [] dummyFA2Contract
  let storageInitial = storageFn addresses
  now_level <- ifEmulation getLevel (getLevel >>= (\x -> pure $ x + 5))
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
    , uodContract = convertContract baseDAOLambdaLigo
    }
  tests addresses storage (TAddress baseDao) (TAddress dodTokenContract)

toPeriod :: LambdaStorage -> Natural
toPeriod = unPeriod . cPeriod . sConfig

-- | A proposal handler check that check the
-- input string is not empty.
proposal_1_check :: HandlerCheck
proposal_1_check =
  L.car #
  (unpackRaw @MText) #
  assertSome [mt|Input cannot be unpacked|] #
  L.size #
  assertNeq0 [mt|Input cannot be empty|] #
  push ()


-- | A proposal handler that accept a text value
-- and inserts it into a list in handler storage.
proposal_1_handler :: ProposalHandler
proposal_1_handler =
  (getField #piPacked_argument) #
  (unpackRaw @MText) #  ifNone dummyHandler insertHandlerStorage
  where
    insertHandlerStorage :: '[MText, PhInput] :-> '[PhOutput]
    insertHandlerStorage =
      dip (toField #piHandler_storage) #
      packRaw #
      push [mt|key0|] #
      mapInsert #
      L.nil @Operation #
      L.swap #
      L.none #
      constructStack @PhOutput

dummyHandler :: '[PhInput] :-> '[PhOutput]
dummyHandler =
  toField #piHandler_storage #
  L.nil @Operation #
  L.swap #
  L.none #
  constructStack @PhOutput

-- | We test non-token entrypoints of the BaseDAO contract here
test_LambdaDAO :: [TestTree]
test_LambdaDAO =
  [ let flushPeriod = 41 in testGroup "LambdaDAO Tests"
      [ testScenario "AddHandler/ExecuteHandler adds/execute handlers/proposals" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithExplictLambdaDAOConfig admin) $
           \(admin:wallet1:_) fs baseDao _ -> do

             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Add_handler $ AddHandlerParam "proposal_handler_1" proposal_1_handler proposal_1_check
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             -- Add a new Handler
             --
             -- Advance one voting period to a proposing stage.
             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams

             let proposalKey = makeProposalKey proposeParams

             advanceToLevel (startLevel + 2 * toPeriod fs)
             withSender wallet1 $
               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 200 wallet1) Nothing]

             -- Advance one voting period
             pstart <- getProposalStartLevel' @'Lambda baseDao proposalKey
             advanceToLevel (pstart + flushPeriod)
             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

            -- Execute the handler
             advanceToLevel (startLevel + 4 * toPeriod fs)

             let refString = [mt|Test string|]

             let proposalMeta' = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw refString)
             let proposalSize' = metadataSize proposalMeta'

             advanceToLevel (startLevel + 5 * toPeriod fs)
             let proposeParams' = ProposeParams wallet1 proposalSize' proposalMeta'
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams'
             let proposalKey' = makeProposalKey proposeParams'

             advanceToLevel (startLevel + 6 * toPeriod fs)
             withSender wallet1 $
               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey' True 200 wallet1) Nothing]

             pstart' <- getProposalStartLevel' @'Lambda baseDao proposalKey'
             advanceToLevel (pstart' + flushPeriod)
             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

             -- Check if the result of proposal_handler_1 execution.
             handlerStorage <- (leHandlerStorageRPC . sExtraRPC) <$> getStorageRPCLambda baseDao
             case M.lookup [mt|key0|] handlerStorage of
              Nothing -> fail "Expected key was not found in HandlerStorage"
              Just x -> case lUnpackValueRaw @MText x of
                Right ux -> assert (ux == refString) "Found unexpected value for key"
                Left _ -> fail "Unpacking failed"

      , testScenario "AddHandler proposal check checks for existing handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithExplictLambdaDAOConfig admin) $
           \(admin:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Add_handler $ AddHandlerParam "proposal_handler_1" proposal_1_handler proposal_1_check
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             -- Add a new Handler
             --
             -- Advance one voting period to a proposing stage.
             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams

             let proposalKey = makeProposalKey proposeParams

             advanceToLevel (startLevel + 2 * toPeriod fs)
             withSender wallet1 $
               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 200 wallet1) Nothing]

             pstart <- getProposalStartLevel' @'Lambda baseDao proposalKey
             advanceToLevel (pstart + flushPeriod)

             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

             -- Raise a proposal to add the duplicate handler
             advanceToLevel (startLevel + 4 * toPeriod fs)
             let proposeParams' = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams'
               & expectFailedWith (failProposalCheck, proposalHandlerExists)

      , testScenario "Remove handler proposal check checks for existing handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithExplictLambdaDAOConfig admin) $
           \(_:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Remove_handler "proposal_handler_1"
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams
               & expectFailedWith (failProposalCheck, proposalHandlerNotFound)

      , testScenario "Remove handler proposal check checks for active handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithPreloadedLambdasDisabled admin) $
           \(_:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Remove_handler "proposal_handler_1"
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams
               & expectFailedWith (failProposalCheck, proposalHandlerNotFound)

      , testScenario "ExecuteHandler proposal check checks for existing handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithExplictLambdaDAOConfig admin) $
           \(_:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw [mt|Something|])
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams
               & expectFailedWith (failProposalCheck, proposalHandlerNotFound)

      , testScenario "ExecuteHandler proposal check checks for active handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithPreloadedLambdasDisabled admin) $
           \(_:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw [mt|Something|])
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams
               & expectFailedWith (failProposalCheck, proposalHandlerNotFound)

      , testScenario "RemoveHandler proposal disables the handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithPreloadedLambdas admin) $
           \(admin:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $ Remove_handler [mt|proposal_handler_1|]
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             -- Advance one voting period to a proposing stage.
             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams

             let proposalKey = makeProposalKey proposeParams

             advanceToLevel (startLevel + 2 * toPeriod fs)
             withSender wallet1 $
               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 200 wallet1) Nothing]

             pstart <- getProposalStartLevel' @'Lambda baseDao proposalKey
             advanceToLevel (pstart + flushPeriod)

             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

            -- Raise an execute "proposal_handler_1" proposal
             advanceToLevel (startLevel + 4 * toPeriod fs)

             let refString = [mt|Test string|]

             let proposalMeta' = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw refString)
             let proposalSize' = metadataSize proposalMeta'

             advanceToLevel (startLevel + 5 * toPeriod fs)
             let proposeParams' = ProposeParams wallet1 proposalSize' proposalMeta'
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams'
               & expectFailedWith (failProposalCheck, proposalHandlerNotFound)

      , testScenario "RemovingHandler does not prevent existing proposals from executing" $ scenario $
         let
          flushInterval = 40
          storageFn adm =
            let
              s = initialStorageWithPreloadedLambdas adm
            in s { sConfig = (sConfig s) { cPeriod = 30, cProposalFlushLevel = flushInterval, cProposalExpiredLevel = 300 } }
         in withOriginated 2
           (\(admin: _) -> storageFn admin) $
           \(admin:wallet1:_) fs baseDao _ -> do
             -- Raise an 'Execute' followed by a 'Remove_handler' proposal
             let refString = [mt|Test string|]
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw refString)
             let proposalSize = metadataSize proposalMeta

             let removeProposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $ Remove_handler $ [mt|proposal_handler_1|]
             let removeProposalSize = metadataSize removeProposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + removeProposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             -- Advance one voting period to a proposing stage.
             let removeProposalRaisedAt = startLevel + toPeriod fs
             -- We plan to raise the execute proposal some levels after raising the remove proposal,
             -- so that the flush call to execute the remove proposal won't flush the execute proposal
             -- with it.
             let executeProposalRaisedAt = removeProposalRaisedAt + 10
             advanceToLevel removeProposalRaisedAt

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             let removeProposeParams = ProposeParams wallet1 removeProposalSize removeProposalMeta
             withSender wallet1 $ do
              call baseDao (Call @"Propose") removeProposeParams
             -- Raise execute proposal. We are still in first proposal period.
             advanceToLevel executeProposalRaisedAt
             withSender wallet1 $ do
              call baseDao (Call @"Propose") proposeParams

             let executeProposalKey = makeProposalKey proposeParams
             let removeProposalKey = makeProposalKey removeProposeParams

             -- Vote for proposals.
             advanceToLevel (startLevel + 2 * toPeriod fs)
             -- We are in second voting period.
             withSender wallet1 $ do
               call baseDao (Call @"Vote") [PermitProtected (VoteParam removeProposalKey True 200 wallet1) Nothing]
               call baseDao (Call @"Vote") [PermitProtected (VoteParam executeProposalKey True 200 wallet1) Nothing]

             advanceToLevel (removeProposalRaisedAt + flushInterval + 10)
             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

             -- We should be in second proposal period now. Because removeProposal was raised at first
             -- proposal period, and flush interval is 2 periods. So one voting period after the first
             -- proposal period have elapsed, and we are in the second proposal period.

             -- Check remove_handler worked by trying to create a new execute proposal for the handler.

             let proposalMeta' = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw [mt|Something|])
             let proposalSize' = metadataSize proposalMeta'

             advanceToLevel (startLevel + 5 * toPeriod fs)
             let proposeParams' = ProposeParams wallet1 proposalSize' proposalMeta'
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams'
               & expectFailedWith (failProposalCheck, proposalHandlerNotFound)

             advanceToLevel (executeProposalRaisedAt + flushInterval + 10)
             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

             -- Check if the result of proposal_handler_1 execution.
             handlerStorage <- (leHandlerStorageRPC . sExtraRPC) <$> getStorageRPCLambda baseDao
             case M.lookup [mt|key0|] handlerStorage of
              Nothing -> fail "Expected key was not found in HandlerStorage"
              Just x -> case lUnpackValueRaw @MText x of
                Right ux -> assert (ux == refString) "Found unexpected value for key"
                Left _ -> fail "Unpacking failed"
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
    fs = baseDAOLambdaStorageLigo
    in fs { sAdmin = admin, sConfig = (sConfig fs)
              { cPeriod = 20
              , cProposalFlushLevel = 40
              , cProposalExpiredLevel = 330
              , cGovernanceTotalSupply = 100

        }}

  initialStorageWithExplictLambdaDAOConfig :: Address -> LambdaStorage
  initialStorageWithExplictLambdaDAOConfig admin = (initialStorage admin)

  initialStorageWithPreloadedLambdas :: Address -> LambdaStorage
  initialStorageWithPreloadedLambdas admin = let
    preloadedHandlerInfo = HandlerInfo proposal_1_handler proposal_1_check True
    in setExtra (\e -> e { leLambdas = mkBigMap [([mt|proposal_handler_1|],  preloadedHandlerInfo)] }  ) (initialStorage admin)

  initialStorageWithPreloadedLambdasDisabled :: Address -> LambdaStorage
  initialStorageWithPreloadedLambdasDisabled admin = let
    preloadedHandlerInfo = HandlerInfo proposal_1_handler proposal_1_check False
    in setExtra (\e -> e { leLambdas = mkBigMap [([mt|proposal_handler_1|],  preloadedHandlerInfo)] }  ) (initialStorage admin)
