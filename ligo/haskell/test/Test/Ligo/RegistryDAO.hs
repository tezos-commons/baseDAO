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

import BaseDAO.ShareTest.Common (totalSupplyFromLedger, makeProposalKey)
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
  baseDao <- originateLargeUntyped $ UntypedOriginateData
    { uodName = "BaseDAO - RegistryDAO Test Contract"
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
            in withSender (AddressResolved wallet1) $ call baseDao (Call @"Propose")
              (ProposeParams proposalSize proposalMeta)

    , nettestScenarioCaps "proposal exceeding max_proposal_size result in error" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            -- In the explicitly set configuration max_proposal_size is set at 100.
            -- And here we create a proposal that is bigger then 100.
            proposalMeta = DynamicRec $ Map.fromList $
              [(mkMTextUnsafe ("long_key" <> (show @_ @Int t)), "long_value") | t <- [1..10]]
            proposalSize = metadataSize proposalMeta
            in withSender (AddressResolved wallet1) $ call
               baseDao (Call @"Propose") (ProposeParams proposalSize proposalMeta)
               & expectFailProposalCheck

    , nettestScenarioCaps "checks it fails if required tokens are not frozen" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            proposalMeta = DynamicRec mempty
            -- Here we only freeze 2 tokens, but the proposal size and the configuration params
            -- frozen_scale_value, frozen_extra_value set to 1 and 0 means that it requires 6
            -- tokens to be frozen (6 * 1 + 0) because proposal size happen to be 6 here.
            in withSender (AddressResolved wallet1) $
               call baseDao (Call @"Propose") (ProposeParams 2 proposalMeta)
               & expectFailProposalCheck

    , nettestScenarioCaps "check it correctly calculates required frozen tokens" $
        withOriginated 2
          (\(admin: wallet1:_) -> setExtra @Natural [mt|frozen_extra_value|] 2 $ initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao -> let
            proposalMeta = DynamicRec mempty
            -- Here the proposal size and the configuration params frozen_scale_value,
            -- frozen_extra_value set to 1 and 2 means that it requires 8 tokens to be
            -- frozen (6 * 1 + 2) because proposal size happen to be 6 here.
            in withSender (AddressResolved wallet1) $
               call baseDao (Call @"Propose") (ProposeParams 8 proposalMeta)

    , nettestScenarioCaps "checks it correctly calculates tokens to unfreeze when rejecting" $ do
        let frozen_scale_value = 2
        let frozen_extra_value = 0
        let slash_scale_value = 1
        let slash_division_value = 2
        withOriginated 2
          (\(admin: wallet1:_) -> setExtra @Natural [mt|slash_division_value|] 2 $
            setExtra @Natural [mt|frozen_scale_value|] frozen_scale_value $
            setExtra @Natural [mt|frozen_extra_value|] frozen_extra_value $
            setExtra @Natural [mt|slash_scale_value|] slash_scale_value $
            setExtra @Natural [mt|slash_division_value|] slash_division_value $ initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $

          \(admin: wallet1: _) _ baseDao -> let
            proposalMeta1 = DynamicRec $ Map.fromList $ [([mt|key1|], "val"), ([mt|key2|], "val")] -- 44
            proposalSize1 = metadataSize proposalMeta1

            in do
              let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value
              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozen proposalMeta1)

              advanceTime (day 12) -- voting period is 11 days.
              withSender (AddressResolved admin) $
                call baseDao (Call @"Flush") (1 :: Natural)

              -- Since we have frozen_scale_value = 2, slash_scale_value = 1 and slash_division_value = 2
              -- After the rejection above, we expect that (2 * proposalSize1/2) tokens
              -- will be returned to the proposer.  So they will have a balance
              -- of 100 - proposalSize1.

              -- Check the balance of wallet1 using 'Balance_of' entrypoint.
              consumer <- originateSimple "consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
              let balanceRequestItem = FA2.BalanceRequestItem { briOwner = wallet1, briTokenId = FA2.theTokenId }
              let balanceRequest = FA2.mkFA2View [balanceRequestItem] consumer
              withSender (AddressResolved wallet1) $ call baseDao (Call @"Balance_of") balanceRequest

              let spent = div (requiredFrozen * slash_scale_value) slash_division_value

              checkStorage (AddressResolved $ unTAddress consumer) (toVal [[FA2.BalanceResponseItem balanceRequestItem (defaultTokenBalance - spent)]])

    , nettestScenarioCaps "checks it correctly executes the proposal that has won" $ do
        let frozen_scale_value = 1
        let frozen_extra_value = 0
        let slash_scale_value = 1
        let slash_division_value = 1
        withOriginated 3
          (\(admin: wallet1: voter1:_) -> setExtra @Natural [mt|slash_division_value|] 2 $
            setExtra @Natural [mt|frozen_scale_value|] frozen_scale_value $
            setExtra @Natural [mt|frozen_extra_value|] frozen_extra_value $
            setExtra @Natural [mt|slash_scale_value|] slash_scale_value $
            setExtra @Natural [mt|max_proposal_size|] 200 $
            setExtra @Natural [mt|slash_division_value|] slash_division_value $ initialStorageWithExplictRegistryDAOConfig admin [wallet1, voter1]) $

          \(admin: wallet1: voter1 : _) _ baseDao -> let
            -- We currently have max_proposal_size of 200, but the following proposal is 317 bytes long.
            largeProposalMeta = DynamicRec $ Map.fromList $ [(mkMTextUnsafe ("long_key" <> (show @_ @Int t)), "long_value") | t <- [1..10]]
            largeProposalSize = metadataSize largeProposalMeta

            in do
              runIO $ putTextLn $ show largeProposalSize
              let requiredFrozen = largeProposalSize * frozen_scale_value + frozen_extra_value

              -- We expect this to fail because max_proposal_size is 200 and proposal size is 317.
              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozen largeProposalMeta)
                & expectFailProposalCheck

              -- We create a new proposal to increase max_proposal_size to largeProposalSize + 1.
              let sMaxUpdateproposalMeta1 = DynamicRec $ Map.fromList
                    [([mt|max_proposal_size|], lPackValueRaw $ Just ((largeProposalSize + 1) :: Natural))
                    ,([mt|frozen_scale_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|frozen_extra_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|slash_scale_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|slash_division_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ]
              let sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
              let requiredFrozenForUpdate = sMaxUpdateproposalSize1 * frozen_scale_value + frozen_extra_value

              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozenForUpdate sMaxUpdateproposalMeta1)

              -- Then we send 2 upvotes for the proposal (as min quorum is 2)
              let proposalKey = makeProposalKey @ProposalMetadataL (ProposeParams requiredFrozenForUpdate sMaxUpdateproposalMeta1) wallet1
              withSender (AddressResolved voter1) $
                call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 2) Nothing]

              advanceTime (day 12) -- voting period is 11 days.
              withSender (AddressResolved admin) $
                call baseDao (Call @"Flush") (1 :: Natural)

              -- Now we expect this to work
              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozen largeProposalMeta)

    , nettestScenarioCaps "checks on-chain view correctly returns the registry value" $ do
        -- The default values assigned from initialStorageWithExplictRegistryDAOConfig function
        withOriginated 3
          (\(admin: wallet1: voter1:_) -> setExtra @Natural [mt|max_proposal_size|] 200 $
              initialStorageWithExplictRegistryDAOConfig admin [wallet1, voter1]) $

          \(admin: wallet1: voter1 : _) _ baseDao -> do
            let
              proposalMeta = DynamicRec $ Map.fromList $
                [ ([mt|updates|], lPackValueRaw [([mt|key|], Just [mt|testVal|])])
                , ([mt|agoraPostID|], lPackValueRaw @Natural 10)
                ]
              proposalSize = metadataSize proposalMeta

            runIO $ putTextLn $ show proposalSize
            let requiredFrozen = proposalSize -- since frozen_scale_value and frozen_scale_value are 1 and 0.

            -- We propose the addition of a new registry key
            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Propose") (ProposeParams requiredFrozen proposalMeta)

            -- Then we send 2 upvotes for the proposal (as min quorum is 2)
            let proposalKey = makeProposalKey @ProposalMetadataL (ProposeParams requiredFrozen proposalMeta) wallet1
            withSender (AddressResolved voter1) $
              call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 2) Nothing]

            advanceTime (day 12) -- voting period is 11 days.
            withSender (AddressResolved admin) $
              call baseDao (Call @"Flush") (1 :: Natural)

            consumer <- originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))

            withSender (AddressResolved voter1) $
              call baseDao (Call @"CallCustom") ([mt|lookup_registry|], lPackValueRaw ([mt|key|], consumer))

            checkStorage (AddressResolved $ unTAddress consumer) (toVal [([mt|key|], Just [mt|testVal|])])

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

      ledger = BigMap $ Map.fromList [((w, FA2.theTokenId), defaultTokenBalance) | w <- wallets]

      newStorage = oldStorage
        { sAdmin = admin
        , sLedger = ledger
        , sTotalSupply = totalSupplyFromLedger ledger
        }
      in fs { fsStorage = newStorage }

    initialStorageWithExplictRegistryDAOConfig :: Address -> [Address] -> FullStorage
    initialStorageWithExplictRegistryDAOConfig admin wallets =
      setExtra @Natural [mt|frozen_scale_value|] 1 $
      setExtra @Natural [mt|frozen_extra_value|] 0 $
      setExtra @Natural [mt|slash_scale_value|] 1 $
      setExtra @Natural [mt|slash_division_value|] 1 $
      setExtra @Natural [mt|max_proposal_size|] 100 (initialStorage admin wallets)

    setExtra :: forall a. NicePackedValue a => MText -> a -> FullStorage -> FullStorage
    setExtra key v (s@FullStorage {..}) = let
     oldExtra = unDynamic $ sExtra fsStorage
     newExtra = Map.insert key (lPackValueRaw v) oldExtra
     newStorage = fsStorage { sExtra = DynamicRec newExtra }
     in s { fsStorage = newStorage }


expectFailProposalCheck
  :: (MonadNettest caps base m)
  => m a -> m ()
expectFailProposalCheck = expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK
