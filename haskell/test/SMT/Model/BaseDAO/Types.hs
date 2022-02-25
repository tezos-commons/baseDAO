-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SMT.Model.BaseDAO.Types
  ( ModelT (..)
  , ModelState (..)
  , SimpleContractType (..)
  , SimpleFA2Contract (..)
  , OtherContract (..)
  , SimpleOperation (..)
  , runModelT
  , getStore
  , getConfig
  , modifyStore
  , execOperation

  , ModelCall (..)
  , ModelSource (..)
  , ModelError (..)
  , contractErrorToModelError
  ) where

import Universum hiding (show)

import Control.Monad.Except (MonadError)
import qualified Data.Map as Map
import Fmt
import Text.Show (show)

import Lorentz hiding (cast, get, not)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Tezos.Address (ContractHash(..))
import Morley.Tezos.Core

import Ligo.BaseDAO.Types
import Ligo.BaseDAO.ErrorCodes

-- | Transformer used in haskell implementation of BaseDAO
newtype ModelT var a = ModelT
  { unModelT :: (ExceptT ModelError $ State (ModelState var)) a
  } deriving newtype (Functor, Applicative, Monad, MonadState (ModelState var), MonadError ModelError)

-- | A type that keep track of blockchain state
data ModelState var = ModelState
  { msFullStorage :: FullStorageSkeleton (VariantToExtra var)
  , msMutez :: Mutez
  -- ^ Dao balance

  , msLevel :: Natural
  , msChainId :: ChainId
  , msSelfAddress :: Address
  , msGovernanceTokenAddress :: Address
  , msContracts :: Map Address SimpleContractType
  -- ^ Keep track of originated contract in the blockchain.
  -- See `SimpleContractType` for detail.

  , msProposalCheck :: (ProposeParams, VariantToExtra var) -> ModelT var ()
  , msRejectedProposalSlashValue :: (Proposal, VariantToExtra var) -> ModelT var Natural
  , msDecisionCallback :: DecisionCallbackInput' (VariantToExtra var) -> ModelT var ([SimpleOperation], VariantToExtra var, Maybe Address)
  , msCustomEps :: VariantToParam var -> ModelT var ()
  } deriving stock (Generic)

-- Needed by `forall`
--
deriving stock instance Show (VariantToExtra var) => Show (ModelState var)

instance Show ((ProposeParams, a) -> ModelT var ()) where
  show _ = "<callback>"
instance Show ((Proposal, a) -> ModelT cep Natural) where
  show _ = "<msRejectedProposalSlashValue>"
instance Show (DecisionCallbackInput' b -> ModelT var ([SimpleOperation], a, Maybe Address)) where
  show _ = "<msDecisionCallback>"
instance {-# INCOHERENT #-} Show (a -> ModelT var ()) where
  show _ = "<callback>"

runModelT :: ModelT cep a -> ModelState cep -> (Either ModelError (ModelState cep))
runModelT (ModelT model) initState =
  runExceptT model
    & (`runState` initState)
    & (\(result, updatedMs) -> case result of
          Left err -> Left err
          Right _ -> Right updatedMs
      )

execOperation :: SimpleOperation -> ModelT cep ()
execOperation op = do
  contracts <- get <&> msContracts

  let (updatedContracts, minusBal) =
        case op of
          FA2TransferOperation addr param tez ->
            case Map.lookup addr contracts of
              Just (SimpleFA2ContractType sfc) ->
                let updatedSc = sfc
                      { sfcStorage = param : (sfc & sfcStorage)
                      , sfcMutez = unsafeAddMutez tez (sfc & sfcMutez)
                      }
                in (Map.insert addr (SimpleFA2ContractType updatedSc) contracts, tez)
              _ ->
                (contracts, toMutez 0)
          OtherOperation addr param tez ->
            case Map.lookup addr contracts of
              Just (OtherContractType oc) ->
                let updatedSc = oc
                      { ocStorage = param : (oc & ocStorage)
                      , ocMutez = unsafeAddMutez tez (oc & ocMutez)
                      }
                in (Map.insert addr (OtherContractType updatedSc) contracts, tez)
              _ ->
                (contracts, toMutez 0)

  currentBal <- get <&> msMutez
  let newBal = case currentBal `subMutez` minusBal of
        Just b -> b
        Nothing -> error "Dao balance is negative."

  modify $ \ms -> ms { msContracts = updatedContracts, msMutez = newBal }

getStore :: ModelT var (StorageSkeleton (VariantToExtra var))
getStore = get <&> msFullStorage <&> fsStorage

getConfig :: ModelT cep Config
getConfig = get <&> msFullStorage <&> fsConfig

modifyStore :: (StorageSkeleton (VariantToExtra var) -> ModelT var (StorageSkeleton (VariantToExtra var))) -> ModelT var ()
modifyStore f = do
  ms :: ModelState var <- get
  updatedStorage :: StorageSkeleton (VariantToExtra var) <- f (fsStorage $ msFullStorage $ ms)
  put $ ms
    { msFullStorage = (ms & msFullStorage)
        { fsStorage = updatedStorage }
    }

-- Simple operation that only allows transfering to `FA2.TransferItem` entrypoint
data SimpleOperation
  = FA2TransferOperation Address [FA2.TransferItem] Mutez
  | OtherOperation Address Text Mutez
  deriving stock (Eq, Show, Generic)

instance Buildable SimpleOperation where
  build = genericF


-- | A simple contract type used to track originate contracts
-- We mostly care about contract that has the FA2 transfer entrypont.
-- Other contract call params will be convert to text.
data SimpleContractType
  = SimpleFA2ContractType SimpleFA2Contract
  | OtherContractType OtherContract
  deriving stock (Eq, Show, Generic)

instance Buildable SimpleContractType where
  build = genericF

data SimpleFA2Contract = SimpleFA2Contract
  { sfcStorage :: [FA2.TransferParams]
  , sfcMutez :: Mutez
  } deriving stock (Eq, Show, Generic)

instance Buildable SimpleFA2Contract where
  build = genericF


data OtherContract = OtherContract
  { ocStorage :: [Text]
  , ocMutez :: Mutez
  } deriving stock (Eq, Show, Generic)

instance Buildable OtherContract where
  build = genericF

-- | Definition of an entrypoint call to emulate in the SMTs
data ModelCall var = ModelCall
  { mcAdvanceLevel :: Maybe Natural -- Advance level before calling the entrypoint
  , mcParameter :: Parameter' (VariantToParam var)
  , mcSource :: ModelSource
  } deriving stock (Generic)

deriving stock instance Show (VariantToParam var) => Show (ModelCall var)

instance Buildable ContractHash where
  build = genericF

instance Buildable (Parameter' (VariantToParam var)) => Buildable (ModelCall var) where
  build = genericF

-- | Definition of an @source@ and @sender@ for a call to emulate in the SMTs
data ModelSource = ModelSource
  { msoSender :: Address
  , msoSource :: Address
  } deriving stock (Eq, Show, Generic)

instance Buildable ModelSource where
  build = genericF

-- | The possible contract errors for the models
data ModelError
  = NOT_DELEGATE
  | EMPTY_FLUSH
  | NOT_ENOUGH_FROZEN_TOKENS
  | NOT_PROPOSING_STAGE
  | PROPOSAL_NOT_UNIQUE
  | PROPOSAL_NOT_EXIST
  | EXPIRED_PROPOSAL
  | MISSIGNED
  | VOTING_STAGE_OVER
  | DROP_PROPOSAL_CONDITION_NOT_MET
  | NOT_ADMIN
  | NOT_PENDING_ADMIN
  | BAD_TOKEN_CONTRACT
  | FAIL_PROPOSAL_CHECK
  | UNSTAKE_INVALID_PROPOSAL
  | VOTER_DOES_NOT_EXIST
  deriving stock (Generic, Eq, Show)

instance Buildable ModelError where
  build = genericF

instance (Buildable a, Buildable b) => Buildable (Either a b) where
  build = genericF

contractErrorToModelError :: Integer -> ModelError
contractErrorToModelError errorCode
  | (errorCode == toInteger notDelegate) = NOT_DELEGATE
  | (errorCode == toInteger emptyFlush) = EMPTY_FLUSH
  | (errorCode == toInteger notEnoughFrozenTokens) = NOT_ENOUGH_FROZEN_TOKENS
  | (errorCode == toInteger notProposingStage) = NOT_PROPOSING_STAGE
  | (errorCode == toInteger proposalNotUnique) = PROPOSAL_NOT_UNIQUE
  | (errorCode == toInteger proposalNotExist) = PROPOSAL_NOT_EXIST
  | (errorCode == toInteger expiredProposal) = EXPIRED_PROPOSAL
  | (errorCode == toInteger missigned) = MISSIGNED
  | (errorCode == toInteger votingStageOver) = VOTING_STAGE_OVER
  | (errorCode == toInteger dropProposalConditionNotMet) = DROP_PROPOSAL_CONDITION_NOT_MET
  | (errorCode == toInteger notAdmin) = NOT_ADMIN
  | (errorCode == toInteger notPendingAdmin) = NOT_PENDING_ADMIN
  | (errorCode == toInteger badTokenContract) = BAD_TOKEN_CONTRACT
  | (errorCode == toInteger failProposalCheck) = FAIL_PROPOSAL_CHECK
  | (errorCode == toInteger unstakeInvalidProposal) = UNSTAKE_INVALID_PROPOSAL
  | (errorCode == toInteger voterDoesNotExist) = VOTER_DOES_NOT_EXIST
  | otherwise = error $ toText $ show errorCode
