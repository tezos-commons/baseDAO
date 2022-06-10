-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Ligo.BaseDAO.LambdaDAO.Types
  ( LambdaStorage
  , LambdaCustomEpParam
  , LambdaDaoProposalMetadata(..)
  , AddHandlerParam(..)
  ) where

import Universum hiding (fromInteger)

import Data.Map qualified as M
import Data.Set qualified as S

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Morley.AsRPC

import Ligo.BaseDAO.Types

-- | Helper type for unpack/pack
data LambdaDaoProposalMetadata
  = Add_handler AddHandlerParam
  | Remove_handler RemoveHandlerParam
  | Execute_handler ExecuteHandlerParam

type RemoveHandlerParam = MText

data ExecuteHandlerParam = ExecuteHandlerParam
  { ehHandlerName :: MText
  , ehPackedArgument :: ByteString
  }

instance HasAnnotation ExecuteHandlerParam where
  annOptions = baseDaoAnnOptions

instance HasAnnotation LambdaDaoProposalMetadata where
  annOptions = baseDaoAnnOptions

type HandlerStorage = Map MText ByteString

data PhInput = PhInput
  { piPacked_argument :: ByteString
  , piHandler_storage :: HandlerStorage
  }

data PhOutput = PhOutput
  { poOperations :: [Operation]
  , poHandlerStorage :: HandlerStorage
  }

type HandlerCheck = Lambda (ProposeParams, HandlerStorage) ()

type ProposalHandler = Lambda PhInput PhOutput

data AddHandlerParam = AddHandlerParam
  { ahName :: MText
  , ahCode :: ProposalHandler
  , ahHandler_check :: HandlerCheck
  }

instance HasAnnotation PhInput where
  annOptions = baseDaoAnnOptions

instance HasAnnotation PhOutput where
  annOptions = baseDaoAnnOptions

instance HasAnnotation AddHandlerParam where
  annOptions = baseDaoAnnOptions

customGeneric "LambdaDaoProposalMetadata" ligoLayout
deriving anyclass instance IsoValue LambdaDaoProposalMetadata

customGeneric "ExecuteHandlerParam" ligoLayout
deriving anyclass instance IsoValue ExecuteHandlerParam

customGeneric "AddHandlerParam" ligoLayout
deriving anyclass instance IsoValue AddHandlerParam

customGeneric "PhInput" ligoLayout
deriving anyclass instance IsoValue PhInput

customGeneric "PhOutput" ligoLayout
deriving anyclass instance IsoValue PhOutput

data LambdaExtra = LambdaExtra
  { leLambdas :: BigMap MText ProposalHandler
  , leHandlerStorage :: HandlerStorage
  } deriving stock (Eq)

instance Default LambdaExtra where
  def = LambdaExtra (mkBigMap @(Map MText ProposalHandler) M.empty) mempty

instance Buildable LambdaExtra where
  build = genericF

customGeneric "LambdaExtra" ligoLayout
deriveRPCWithStrategy "LambdaExtra" ligoLayout

deriving anyclass instance IsoValue LambdaExtra
instance HasAnnotation LambdaExtra where
  annOptions = baseDaoAnnOptions

type instance VariantToParam 'Lambda = ()
type instance VariantToExtra 'Lambda = LambdaExtra

type LambdaStorage = StorageSkeleton (VariantToExtra 'Lambda)

type LambdaCustomEpParam = ()
