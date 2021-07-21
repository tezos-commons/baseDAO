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

  , ContractType (..)
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

import Michelson.Text
import Lorentz hiding (not, cast, get)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Tezos.Address (ContractHash(..))
import Tezos.Core

import Ligo.BaseDAO.Types

-- | Transformer used in haskell implementation of BaseDAO
newtype ModelT a = ModelT
  { unModelT :: (ExceptT ModelError $ State ModelState) a
  } deriving newtype (Functor, Applicative, Monad, MonadState ModelState, MonadError ModelError)

-- | A type that keep track of blockchain state
data ModelState = ModelState
  { msFullStorage :: FullStorage
  , msMutez :: Mutez
  -- ^ Dao balance

  , msLevel :: Natural
  , msChainId :: ChainId
  , msSelfAddress :: Address
  , msGovernanceTokenAddress :: Address
  , msContracts :: Map Address SimpleContractType
  -- ^ Keep track of originated contract in the blockchain.
  -- See `SimpleContractType` for detail.

  , msProposalCheck :: (ProposeParams, ContractExtra) -> ModelT ()
  , msRejectedProposalSlashValue :: (Proposal, ContractExtra) -> ModelT Natural
  , msDecisionLambda :: DecisionLambdaInput BigMap -> ModelT ([SimpleOperation], ContractExtra, Maybe Address)
  , msCustomEps :: Map MText (ByteString -> ModelT ())
  } deriving stock (Generic, Show)

-- Needed by `forall`
instance Show ((ProposeParams, ContractExtra) -> ModelT ()) where
  show _ = "<msProposalCheck>"
instance Show ((Proposal, ContractExtra) -> ModelT Natural) where
  show _ = "<msRejectedProposalSlashValue>"
instance Show (DecisionLambdaInput BigMap -> ModelT ([SimpleOperation], ContractExtra, Maybe Address)) where
  show _ = "<msDecisionLambda>"
instance Show (ByteString -> ModelT ()) where
  show _ = "<customEps>"

runModelT :: ModelT a -> ModelState -> (Either ModelError ModelState)
runModelT (ModelT model) initState =
  runExceptT model
    & (`runState` initState)
    & (\(result, updatedMs) -> case result of
          Left err -> Left err
          Right _ -> Right updatedMs
      )

execOperation :: SimpleOperation -> ModelT ()
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

getStore :: ModelT Storage
getStore = get <&> msFullStorage <&> fsStorage

getConfig :: ModelT Config
getConfig = get <&> msFullStorage <&> fsConfig

modifyStore :: (Storage -> ModelT Storage) -> ModelT ()
modifyStore f = do
  ms <- get
  updatedStorage <- f (ms & msFullStorage & fsStorage)
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
data ModelCall = ModelCall
  { mcAdvanceLevel :: Maybe Natural -- Advance level before calling the entrypoint
  , mcParameter :: Parameter
  , mcSource :: ModelSource
  } deriving stock (Eq, Show, Generic)

instance Buildable ContractHash where
  build = genericF

instance Buildable ModelCall where
  build = genericF

-- | Definition of an @source@ and @sender@ for a call to emulate in the SMTs
data ModelSource = ModelSource
  { msoSender :: Address
  , msoSource :: Address
  } deriving stock (Eq, Show, Generic)

instance Buildable ModelSource where
  build = genericF

-- | We need this type since we need to do certain things for specific dao
-- Ex. `CallCustom`, Needed to `sendXtz` for treasury/registry dao
data ContractType
  = BaseDaoContract
  | RegistryDaoContract
  | TreasuryDaoContract
  deriving stock Eq

-- | The possible contract errors for the models
data ModelError
  = NOT_DELEGATE
  | BAD_ENTRYPOINT_PARAMETER
  | EMPTY_FLUSH
  | MAX_PROPOSALS_REACHED
  | NOT_ENOUGH_FROZEN_TOKENS
  | NOT_PROPOSING_STAGE
  | PROPOSAL_NOT_UNIQUE
  | PROPOSAL_NOT_EXIST
  | EXPIRED_PROPOSAL
  | MISSIGNED
  | MAX_VOTERS_REACHED
  | VOTING_STAGE_OVER
  | DROP_PROPOSAL_CONDITION_NOT_MET
  | NOT_ADMIN
  | NOT_PENDING_ADMIN
  | BAD_TOKEN_CONTRACT
  | ENTRYPOINT_NOT_FOUND
  | UNPACKING_FAILED
  | FAIL_PROPOSAL_CHECK
  | MISSING_VALUE
  deriving stock (Generic, Eq, Show)

instance Buildable ModelError where
  build = genericF

instance (Buildable a, Buildable b) => Buildable (Either a b) where
  build = genericF

instance (Buildable a, Buildable b) => Buildable (a, b) where
  build  = genericF

instance (Buildable a, Buildable b, Buildable c) => Buildable (a, b, c) where
  build  = genericF

instance (Buildable a, Buildable b, Buildable c, Buildable d) => Buildable (a, b, c, d) where
  build  = genericF

contractErrorToModelError :: Text -> ModelError
contractErrorToModelError txtError =
  case txtError of
    "NOT_DELEGATE" -> NOT_DELEGATE
    "BAD_ENTRYPOINT_PARAMETER" -> BAD_ENTRYPOINT_PARAMETER
    "EMPTY_FLUSH" -> EMPTY_FLUSH
    "MAX_PROPOSALS_REACHED" -> MAX_PROPOSALS_REACHED
    "NOT_ENOUGH_FROZEN_TOKENS" -> NOT_ENOUGH_FROZEN_TOKENS
    "NOT_PROPOSING_STAGE" -> NOT_PROPOSING_STAGE
    "PROPOSAL_NOT_UNIQUE" -> PROPOSAL_NOT_UNIQUE
    "PROPOSAL_NOT_EXIST" -> PROPOSAL_NOT_EXIST
    "EXPIRED_PROPOSAL" -> EXPIRED_PROPOSAL
    "MISSIGNED" -> MISSIGNED
    "VOTING_STAGE_OVER" -> VOTING_STAGE_OVER
    "DROP_PROPOSAL_CONDITION_NOT_MET" -> DROP_PROPOSAL_CONDITION_NOT_MET
    "NOT_ADMIN" -> NOT_ADMIN
    "NOT_PENDING_ADMIN" -> NOT_PENDING_ADMIN
    "BAD_TOKEN_CONTRACT" -> BAD_TOKEN_CONTRACT
    "ENTRYPOINT_NOT_FOUND" -> ENTRYPOINT_NOT_FOUND
    "UNPACKING_FAILED" -> UNPACKING_FAILED
    "FAIL_PROPOSAL_CHECK" -> FAIL_PROPOSAL_CHECK
    _ -> error txtError
