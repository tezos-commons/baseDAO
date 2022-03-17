-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module SMT.Common.Run
  ( runBaseDaoSMT
  ) where

import Universum hiding (drop, swap)

import qualified Data.Map as Map
import Data.Typeable (eqT, (:~:)(..))
import Fmt (Buildable, build, pretty, unlinesF)
import Hedgehog hiding (assert)

import Lorentz hiding (assert, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Morley.Micheline as MM
import Morley.Michelson.Runtime.Dummy (dummyLevel)
import qualified Morley.Michelson.Typed as T
import qualified Morley.Michelson.Untyped as U
import Test.Cleveland
import Test.Cleveland.Internal.Abstract (ExpressionOrTypedValue(..), TransferFailure(..))
import Test.Cleveland.Lorentz (contractConsumer)

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.RegistryDAO.Types as Registry
import Ligo.BaseDAO.Types
import SMT.Common.Gen
import SMT.Common.Types
import SMT.Model.BaseDAO.Contract
import SMT.Model.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

-- | The functions run the generator to get a list of entrypoints
-- and do setups before calling entrypoints againts ligo and haskell model
-- For Ligo:
--   - Setup initial level (generated by the generator)
--   - Originate auxiliary contracts
--   - Originate basedao contract
-- For Haskell:
--   - Setup `ModelState`
runBaseDaoSMT :: forall var. (Typeable var, IsoValue (VariantToExtra var), Default (VariantToExtra var), T.HasNoOp (ToT (VariantToExtra var))) => SmtOption var -> PropertyT IO ()
runBaseDaoSMT option@SmtOption{..} = do

  -- Run the generator to get a function that will generate a list of entrypoint calls.
  mkModelInput <- forAll (runGeneratorT (genMkModelInput @var option) $ initGeneratorState soMkPropose)

  testScenarioProps $
    (scenarioEmulated $ do
        -- Originate auxiliary contracts
        guardianContract <- (TAddress . toAddress) <$> originateSimple "guardian" () dummyGuardianContract
        tokenContract <- ((TAddress @FA2.Parameter) . toAddress) <$> originateSimple "TokenContract" [] dummyFA2Contract
        registryDaoConsumer <- (TAddress . toAddress) <$> originateSimple "registryDaoConsumer" []
          (contractConsumer @(MText, (Maybe MText))) -- Used in registry dao.

        -- Generate a list of entrypoint calls
        let
          ModelInput (contractCalls, ms) = mkModelInput $ ModelInputArg
              { miaGuardianAddr = unTAddress guardianContract
              , miaGovAddr = unTAddress tokenContract
              , miaViewContractAddr = registryDaoConsumer
              }

        -- Sync current level to start level in contract initial storage
        -- as well as in the model state
        let currentLevel = (dummyLevel + (ms & msLevel))

        let (fullStorage :: FullStorageSkeleton (VariantToExtra var)) = msFullStorage ms
        let storage :: StorageSkeleton (VariantToExtra var) = (fsStorage fullStorage) { sStartLevel = currentLevel, sExtra = sExtra (fsStorage fullStorage) }

        -- Set initial level for the Nettest
        advanceToLevel currentLevel

        -- Modify `FullStorage` from the generator with registry/treasury configuration.
        let newMs :: ModelState var = ms { msLevel = currentLevel, msFullStorage = soModifyFs (fullStorage { fsStorage = storage }) }

        -- Originate Dao for Nettest
        dao <- case soContractType of
          BaseDaoContract ->
            originateUntypedSimple "BaseDAO" (T.untypeValue $ toVal (newMs & msFullStorage)) (T.convertContract baseDAOContractLigo)
          RegistryDaoContract ->
            originateUntypedSimple "BaseDAO" (T.untypeValue $ toVal (newMs & msFullStorage)) (T.convertContract baseDAORegistryLigo)
          TreasuryDaoContract ->
            originateUntypedSimple "BaseDAO" (T.untypeValue $ toVal (newMs & msFullStorage)) (T.convertContract baseDAOTreasuryLigo)

        -- Send some mutez to registry/treasury dao since they can run out of mutez
        newBal <-
          if (soContractType == RegistryDaoContract || soContractType == TreasuryDaoContract) then do
            let bal = toMutez 500
            sendXtzWithAmount bal (TAddress dao)
            pure bal
          else pure (toMutez 0)


        -- Preparing proper `ModelState` to be used in Haskell model
        let newMs_ = newMs
              { msSelfAddress = toAddress dao
              , msContracts = Map.fromList
                  [ ((unTAddress tokenContract), SimpleFA2ContractType $ SimpleFA2Contract [] (toMutez 0))
                  , ((unTAddress registryDaoConsumer), OtherContractType $ OtherContract [] (toMutez 0))
                  ]
              , msMutez = newBal
              , msLevel = currentLevel
              }

        -- Call ligo dao and run haskell model then compare the results.
        case eqT @var @'Base of
          Just Refl ->
            handleCallLoop @'Base (TAddress dao, tokenContract, registryDaoConsumer) contractCalls newMs_
          Nothing -> case eqT @var @'Registry of
            Just Refl ->
              handleCallLoop @'Registry (TAddress dao, tokenContract, registryDaoConsumer) contractCalls newMs_
            Nothing -> case eqT @var @'Treasury of
              Just Refl -> handleCallLoop @'Treasury (TAddress dao, tokenContract, registryDaoConsumer) contractCalls newMs_
              Nothing -> error "Unknown contract"
    )

-- | For each generated entrypoint calls, this function does 3 things:
-- 1. Run haskell model against the call.
-- 2. Call ligo dao with the call
-- 3. Compare the result. If it is to be expected, loop to the next call, else throw the error.
handleCallLoop
  :: (ContractExtraConstrain (VariantToExtra var), Eq (VariantToExtra var), Buildable (VariantToExtra var), Buildable (VariantToParam var), CallCustomEp (VariantToParam var), HasBaseDAOEp (Parameter' (VariantToParam var)), MonadEmulated caps base m)
  => (TAddress (Parameter' (VariantToParam var)), TAddress FA2.Parameter, TAddress (MText, (Maybe MText)))
  -> [ModelCall var] -> ModelState var -> m ()
handleCallLoop _ [] _ = pure ()
handleCallLoop (dao, gov, viewC) (mc:mcs) ms = do

  -- All values here are needed for `printResult`. See `printResult` for the usage of the values.
  let (haskellErrors, updatedMs) = handleCallViaHaskell mc ms
      haskellStoreE = case haskellErrors of
        Just err -> Left err
        Nothing -> Right (fsStorage $ msFullStorage updatedMs)
      haskellDaoBalance = msMutez updatedMs

      govContract = updatedMs & msContracts
        & Map.lookup (unTAddress gov)
        & fromMaybe (error "Governance contract does not exist")
      haskellGovStore = case govContract of SimpleFA2ContractType c -> c & sfcStorage; _ -> error "Shouldn't happen."
      haskellGovBalance = case govContract of SimpleFA2ContractType c -> c & sfcMutez; _ -> error "Shouldn't happen."

      viewContract = updatedMs & msContracts
        & Map.lookup (unTAddress viewC)
        & fromMaybe (error "View contract does not exist")
      haskellViewStore = case viewContract of OtherContractType c -> c & ocStorage; _ -> error "Shouldn't happen."

  (ligoStoreE, ligoDaoBalance, ligoGovStore, ligoGovBalance, ligoViewStore)
    <- handleCallViaLigo (dao, gov, viewC) mc

  printResult mc
    (haskellStoreE, haskellDaoBalance, haskellGovStore, haskellGovBalance, haskellViewStore)
    (ligoStoreE, ligoDaoBalance, ligoGovStore, ligoGovBalance, ligoViewStore)

  handleCallLoop (dao, gov, viewC) mcs updatedMs


-- | Compare dao's storage, error, and balance, and its auxiliary contracts:
--   - an fa2 contract storage and mutez (which is gov contract in this case)
--   - a view contract storage
-- Note: Gov contract does not necessarily have to be the governance contract of basedao.
-- We simply need a FA2 contract to do various operation in the haskell model, and gov contract
-- just happen to be a convenience FA2 contract that we can use.
printResult
  :: (Buildable (VariantToParam var), Buildable (VariantToExtra var), Eq (VariantToExtra var), MonadEmulated caps base m)
  => ModelCall var
  -> (Either ModelError (StorageSkeleton (VariantToExtra var)), Mutez, [FA2.TransferParams], Mutez, [Text])
  -> (Either ModelError (StorageSkeleton (VariantToExtra var)), Mutez, [FA2.TransferParams], Mutez, [Text])
  -> m ()
printResult mc
  (haskellStoreE, haskellDaoBalance, haskellGovStore, haskellGovBalance, haskellViewStore)
  (ligoStoreE, ligoDaoBalance, ligoGovStore, ligoGovBalance, ligoViewStore) = do

    assert (haskellStoreE == ligoStoreE) $
      unlinesF
        [ "━━ Error: Haskell and Ligo storage are different ━━"
        , modelCallMsg
        , "━━ Haskell storage ━━"
        , build haskellStoreE
        , "━━ Ligo storage ━━"
        , build ligoStoreE
        ]

    -- Dao contract balance could be updated via treasury/registry xtz proposal.
    assert (haskellDaoBalance == ligoDaoBalance) $
      unlinesF
        [ "━━ Error: Haskell and Ligo dao contract balance are different ━━"
        , modelCallMsg
        , "━━ Haskell dao contract balance ━━"
        , build haskellDaoBalance
        , "━━ Ligo dao contract balance ━━"
        , build ligoDaoBalance
        ]

    assert (haskellGovStore == ligoGovStore) $
      unlinesF
        [ "━━ Error: Haskell and Ligo governance contract storage are different ━━"
        , modelCallMsg
        , "━━ Haskell governance contract storage ━━"
        , build haskellGovStore
        , "━━ Ligo governance contract storage ━━"
        , build ligoGovStore
        ]

    -- Governance contract balance could be updated via treasury/registry transfer proposal.
    assert (haskellGovBalance == ligoGovBalance) $
      unlinesF
        [ "━━ Error: Haskell and Ligo governance contract balance are different ━━"
        , modelCallMsg
        , "━━ Haskell governance contract balance ━━"
        , build haskellGovBalance
        , "━━ Ligo governance contract balance ━━"
        , build ligoGovBalance
        ]

    -- View contract storage could be updated via registry lookup registry call.
    assert (haskellViewStore == ligoViewStore) $
      unlinesF
        [ "━━ Error: Haskell and Ligo view contract storage are different ━━"
        , modelCallMsg
        , "━━ Haskell view contract storage ━━"
        , build haskellViewStore
        , "━━ Ligo view contract storage ━━"
        , build ligoViewStore
        ]

    where
      modelCallMsg = "* Call with:\n" <> (pretty mc)


type HasBaseDAOEp a = (HasDefEntrypointArg a (EntrypointRef 'Nothing) (), ParameterContainsEntrypoints a
  '[ "Propose" :> ProposeParams
   , "Transfer_contract_tokens" :> TransferContractTokensParam
   , "Transfer_ownership" :> TransferOwnershipParam
   , "Accept_ownership" :> ()
   , "Vote" :> [PermitProtected VoteParam]
   , "Flush" :> Natural
   , "Unfreeze" :> UnfreezeParam
   , "Freeze" :> FreezeParam
   , "Drop_proposal" :> ProposalKey
   , "Update_delegate" :> [DelegateParam]
   , "Unstake_vote" :> UnstakeVoteParam
   ])

-- | Advance nettest level and call ligo dao with the provided argument.
-- Return the result of the call (storage or error) and the storage of
-- auxiliary contracts.
handleCallViaLigo
  :: (ContractExtraConstrain (VariantToExtra var), CallCustomEp (VariantToParam var), HasBaseDAOEp (Parameter' (VariantToParam var)), MonadEmulated caps base m)
  => (TAddress (Parameter' (VariantToParam var)), TAddress FA2.Parameter, TAddress (MText, Maybe MText))
  -> ModelCall var
  -> m (Either ModelError (StorageSkeleton (VariantToExtra var)), Mutez, [FA2.TransferParams], Mutez, [Text])
handleCallViaLigo (dao, gov, viewC) mc = do
  case (mc & mcAdvanceLevel) of
    Just lvl -> advanceLevel lvl
    Nothing -> pure ()

  nettestResult <- attempt @TransferFailure $ callLigoEntrypoint mc dao
  let result = parseNettestError nettestResult
  fs <- getFullStorage (unTAddress dao)
  let fsE = case result of
        Just err -> Left err
        Nothing -> Right (fs & fsStorage)

  daoBalance <- getBalance dao

  govStore <- getFullStorage @([FA2.TransferParams]) (unTAddress gov)
  govBalance <- getBalance gov

  viewStorage <- getFullStorage @([(MText, Maybe MText)]) (unTAddress viewC)
  pure (fsE, daoBalance, govStore, govBalance, show <$> viewStorage)


callLigoEntrypoint ::
  ( CallCustomEp (VariantToParam var), HasBaseDAOEp (Parameter' (VariantToParam var))
  , MonadCleveland caps base m
  ) => ModelCall var -> TAddress (Parameter' (VariantToParam var)) -> m ()
callLigoEntrypoint mc dao = withSender (mc & mcSource & msoSender) $ case mc & mcParameter of
  XtzAllowed (ConcreteEp (Propose p)) -> call dao (Call @"Propose") p
  XtzAllowed (ConcreteEp (Transfer_contract_tokens p)) -> call dao (Call @"Transfer_contract_tokens") p
  XtzAllowed (ConcreteEp (Transfer_ownership p)) -> call dao (Call @"Transfer_ownership") p
  XtzAllowed (ConcreteEp (Accept_ownership p)) -> call dao (Call @"Accept_ownership") p
  XtzAllowed (ConcreteEp (Default _)) -> call dao CallDefault ()

  XtzForbidden (Vote p) -> call dao (Call @"Vote") p
  XtzForbidden (Flush p) -> call dao (Call @"Flush") p
  XtzForbidden (Freeze p) -> call dao (Call @"Freeze") p
  XtzForbidden (Unfreeze p) -> call dao (Call @"Unfreeze") p
  XtzForbidden (Update_delegate p) -> call dao (Call @"Update_delegate") p
  XtzForbidden (Drop_proposal p) -> call dao (Call @"Drop_proposal") p
  XtzForbidden (Unstake_vote p) -> call dao (Call @"Unstake_vote") p

  XtzAllowed (CustomEp p) -> callCustomEp dao p

class CallCustomEp p where
  callCustomEp :: MonadCleveland caps base m => (TAddress (Parameter' p)) -> p -> m ()

instance CallCustomEp () where
  callCustomEp _ _ = pure ()

instance CallCustomEp RegistryCustomEpParam where
  callCustomEp dao p = case p of
    Lookup_registry p_ -> call dao (Call @"Lookup_registry") p_
    _ -> pure ()

-- TODO: Use `fromExpression` instead when new morley version is updated.
-- More detail: https://github.com/tezos-commons/baseDAO/pull/282#discussion_r669572842
parseNettestError :: Either TransferFailure a -> Maybe ModelError
parseNettestError = \case
  Right _ -> Nothing
  Left (FailedWith _ (EOTVExpression expr)) -> case MM.fromExpression @U.Value expr of
    Right (U.ValueInt err) -> Just $ contractErrorToModelError err
    Right (U.ValuePair (U.ValueInt err) _) -> Just $ contractErrorToModelError err
    err -> error $ "Unexpected error:" <> show err
  Left (FailedWith _ (EOTVTypedValue (T.VNat tval))) ->
    Just $ contractErrorToModelError $ toInteger tval
  Left (FailedWith _ (EOTVTypedValue (T.VPair (T.VNat tval, _)))) ->
    Just $ contractErrorToModelError $ toInteger tval
  Left err -> error $ "Unexpected error:" <> show err
