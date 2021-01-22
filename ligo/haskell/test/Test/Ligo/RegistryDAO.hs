-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO
  ( test_RegistryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Test.Tasty (TestTree, testGroup)
import Time (day)

import Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test.Consumer
import Michelson.Text (mkMTextUnsafe)
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import BaseDAO.ShareTest.Common (makeProposalKey)
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Ligo.Util

withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> FullStorage -> TAddress ParameterL -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  let storage = storageFn addresses
  baseDao <- originateUntyped $ UntypedOriginateData
    { uodFrom = nettestAddress
    , uodName = "BaseDAO - RegistryDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal storage
    , uodContract = convertContract baseDAOContractLigo
    }
  tests addresses storage (TAddress baseDao)

-- | We test non-token entrypoints of the BaseDAO contract here
test_RegistryDAO :: [TestTree]
test_RegistryDAO =
  [ testGroup "RegistryDAO Tests"
    [ nettestScenarioCaps "Calling the propose endpoint with an empty proposal works" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            proposalMeta = DynamicRec mempty
            proposalSize = metadataSize proposalMeta
            in callFrom (AddressResolved wallet1) baseDao (Call @"Propose")
              (ProposeParams proposalSize proposalMeta)

    , nettestScenarioCaps "proposal exceeding s_max result in error" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            -- In the explicitly set configuration s_max is set at 100.
            -- And here we create a proposal that is bigger then 100.
            proposalMeta = DynamicRec $ Map.fromList $
              [(mkMTextUnsafe ("long_key" <> (show @_ @Int t)), "long_value") | t <- [1..10]]
            proposalSize = metadataSize proposalMeta
            in callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams proposalSize proposalMeta)
              & expectFailProposalCheck

    , nettestScenarioCaps "checks it fails if required tokens are not frozen" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            proposalMeta = DynamicRec mempty
            -- Here we only freeze 2 tokens, but the proposal size and the configuration params
            -- a, b set to 1 and 0 means that it requires 6 tokens to be frozen (6 * 1 + 0)
            -- because proposal size happen to be 6 here.
            in callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams 2 proposalMeta)
              & expectFailProposalCheck

    , nettestScenarioCaps "check it correctly calculates required frozen tokens" $
        withOriginated 2
          (\(admin: wallet1:_) -> setExtra @Natural [mt|b|] 2 $ initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            proposalMeta = DynamicRec mempty
            -- Here the proposal size and the configuration params
            -- a, b set to 1 and 2 means that it requires 8 tokens to be frozen (6 * 1 + 2)
            -- because proposal size happen to be 6 here.
            in callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams 8 proposalMeta)

    , nettestScenarioCaps "checks it correctly calculates tokens to unfreeze when rejecting" $ do
        let a = 2
        let b = 0
        let c = 1
        let d = 2
        withOriginated 2
          (\(admin: wallet1:_) -> setExtra @Natural [mt|d|] 2 $
            setExtra @Natural [mt|a|] a $
            setExtra @Natural [mt|b|] b $
            setExtra @Natural [mt|c|] c $
            setExtra @Natural [mt|d|] d $ initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $

          \(admin: wallet1: _) _ baseDao -> let
            proposalMeta1 = DynamicRec $ Map.fromList $ [([mt|key1|], "val"), ([mt|key2|], "val")] -- 44
            proposalSize1 = metadataSize proposalMeta1

            in do
              let requiredFrozen = proposalSize1 * a + b
              callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams requiredFrozen proposalMeta1)

              advanceTime (day 12) -- voting period is 11 days.
              callFrom (AddressResolved admin) baseDao (Call @"Flush") (1 :: Natural)

              -- Since we have a = 2, c = 1 and d = 2
              -- After the rejection above, we expect that (2 * proposalSize1/2) tokens
              -- will be returned to the proposer.  So they will have a balance
              -- of 100 - proposalSize1.

              -- Check the balance of wallet1 using 'Balance_of' entrypoint.
              consumer <- originateSimple "consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
              let balanceRequestItem = FA2.BalanceRequestItem { briOwner = wallet1, briTokenId = FA2.theTokenId }
              let balanceRequest = FA2.mkFA2View [balanceRequestItem] consumer
              callFrom (AddressResolved wallet1) baseDao (Call @"Balance_of") balanceRequest

              let spent = div (requiredFrozen * c) d

              checkStorage (AddressResolved $ unTAddress consumer) (toVal [[FA2.BalanceResponseItem balanceRequestItem (defaultTokenBalance - spent)]])

    , nettestScenarioCaps "checks it correctly executes the proposal that has won" $ do
        let a = 1
        let b = 0
        let c = 1
        let d = 1
        withOriginated 3
          (\(admin: wallet1: voter1:_) -> setExtra @Natural [mt|d|] 2 $
            setExtra @Natural [mt|a|] a $
            setExtra @Natural [mt|b|] b $
            setExtra @Natural [mt|c|] c $
            setExtra @Natural [mt|s_max|] 100 $
            setExtra @Natural [mt|d|] d $ initialStorageWithExplictRegistryDAOConfig admin [wallet1, voter1]) $

          \(admin: wallet1: voter1 : _) _ baseDao -> let
            -- We currently have s_max of 100, but the following proposal is ~  320 bytes long.
            largeProposalMeta = DynamicRec $ Map.fromList $ [(mkMTextUnsafe ("long_key" <> (show @_ @Int t)), "long_value") | t <- [1..10]]
            largeProposalSize = metadataSize largeProposalMeta

            in do
              runIO $ putTextLn $ show largeProposalSize
              let requiredFrozen = largeProposalSize * a + b

              -- We expect this to fail because s_max is 100 and proposal size is ~ 320.
              callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams requiredFrozen largeProposalMeta)
                & expectFailProposalCheck

              -- We create a new proposal to increase s_max to largeProposalSize + 1.
              let sMaxUpdateproposalMeta1 = DynamicRec $ Map.fromList
                    [([mt|s_max|], lPackValueRaw $ Just ((largeProposalSize + 1) :: Natural))
                    ,([mt|a|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|b|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|c|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|d|], lPackValueRaw @(Maybe Natural) Nothing)
                    ]
              let sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
              let requiredFrozenForUpdate = sMaxUpdateproposalSize1 * a + b

              callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams requiredFrozenForUpdate sMaxUpdateproposalMeta1)

              -- Then we send 2 upvotes for the proposal (as min quorum is 2)
              let proposalKey = makeProposalKey @ProposalMetadataL (ProposeParams requiredFrozenForUpdate sMaxUpdateproposalMeta1) wallet1
              callFrom (AddressResolved voter1) baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 2) Nothing]

              advanceTime (day 12) -- voting period is 11 days.
              callFrom (AddressResolved admin) baseDao (Call @"Flush") (1 :: Natural)

              -- Now we expect this to work
              callFrom (AddressResolved wallet1) baseDao (Call @"Propose") (ProposeParams requiredFrozen largeProposalMeta)
    ]
 ]
 where

   defaultTokenBalance :: Natural = 1000

   metadataSize :: DynamicRec "pm" -> Natural
   metadataSize md = fromIntegral $ BS.length $ lPackValueRaw md

   -- Here we parse the storage value from compiled ligo storage, which
   -- contains the RegistryDAO lambdas implemented in LIGO, and we just use
   -- `fromVal` to convert it to a 'FullStorage'. Then we can set the
   -- RegistryDAO configuration values using the setExtra function below, and
   -- initialize the contract using it. This let us have the lambdas from LIGO
   -- in storage, and allows to tweak RegistryDAO configuration in tests.
   initialStorage :: Address -> [Address] -> FullStorage
   initialStorage admin wallets = let
      fs = fromVal ($(fetchValue @FullStorage "ligo/haskell/test/registryDAO_storage.tz" "REGISTRY_STORAGE_PATH"))
      oldStorage = fsStorage fs
      newStorage = oldStorage { sAdmin = admin, sLedger = BigMap $ Map.fromList [((w, FA2.theTokenId), defaultTokenBalance) | w <- wallets] }
      in fs { fsStorage = newStorage }

   initialStorageWithExplictRegistryDAOConfig :: Address -> [Address] -> FullStorage
   initialStorageWithExplictRegistryDAOConfig admin wallets =
      setExtra @Natural [mt|a|] 1 $
      setExtra @Natural [mt|b|] 0 $
      setExtra @Natural [mt|c|] 1 $
      setExtra @Natural [mt|d|] 1 $
      setExtra @Natural [mt|s_max|] 100 (initialStorage admin wallets)

   setExtra :: forall a. NicePackedValue a => MText -> a -> FullStorage -> FullStorage
   setExtra key v (s@FullStorage {..}) = let
    oldExtra = unDynamic $ sExtra fsStorage
    newExtra = Map.insert key (lPackValueRaw v) oldExtra
    newStorage = fsStorage { sExtra = DynamicRec newExtra }
    in s { fsStorage = newStorage }


expectFailProposalCheck
  :: (MonadNettest caps base m)
  => m a -> m ()
expectFailProposalCheck = expectCustomError_ #fAIL_PROPOSAL_CHECK
