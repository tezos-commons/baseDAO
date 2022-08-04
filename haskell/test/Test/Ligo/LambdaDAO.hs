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
  [ testGroup "LambdaDAO Tests"
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
             advanceToLevel (startLevel + 3 * toPeriod fs)
             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

            -- Execute the handler
             advanceToLevel (startLevel + 4 * toPeriod fs)

             let refString = [mt|Test string|]

             let proposalMeta' = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" (lPackValueRaw @MText refString)
             let proposalSize' = metadataSize proposalMeta'

             advanceToLevel (startLevel + 5 * toPeriod fs)
             let proposeParams' = ProposeParams wallet1 proposalSize' proposalMeta'
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams'
             let proposalKey' = makeProposalKey proposeParams'

             advanceToLevel (startLevel + 6 * toPeriod fs)
             withSender wallet1 $
               call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey' True 200 wallet1) Nothing]

             advanceToLevel (startLevel + 7 * toPeriod fs)
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

             -- Advance one voting period
             advanceToLevel (startLevel + 3 * toPeriod fs)
             withSender admin $
               call baseDao (Call @"Flush") (1 :: Natural)

             -- Raise a proposal to add the duplicate handler
             advanceToLevel (startLevel + 4 * toPeriod fs)
             let proposeParams' = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams'
               & expectFailedWith proposalHandlerExists

      , testScenario "RemoveHandler proposal check checks for existing handler" $ scenario $
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
               & expectFailedWith proposalHandlerNotFound

      , testScenario "ExecuteHandler proposal check checks for existing handler" $ scenario $
         withOriginated 2
           (\(admin: _) -> initialStorageWithExplictLambdaDAOConfig admin) $
           \(_:wallet1:_) fs baseDao _ -> do
             let proposalMeta = lPackValueRaw @LambdaDaoProposalMetadata $
                   Execute_handler $ ExecuteHandlerParam "proposal_handler_1" mempty
             let proposalSize = metadataSize proposalMeta
             withSender wallet1 $
               call baseDao (Call @"Freeze") (#amount :! (proposalSize + 1000))
             startLevel <- getOriginationLevel' @'Lambda baseDao

             advanceToLevel (startLevel + toPeriod fs)

             let proposeParams = ProposeParams wallet1 proposalSize proposalMeta
             withSender wallet1 $ call baseDao (Call @"Propose") proposeParams
               & expectFailedWith proposalHandlerNotFound
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
