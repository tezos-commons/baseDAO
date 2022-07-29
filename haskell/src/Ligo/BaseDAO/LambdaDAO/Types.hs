-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
{-# OPTIONS_GHC -Wno-orphans #-}

module Ligo.BaseDAO.LambdaDAO.Types
  ( AddHandlerParam(..)
  , ExecuteHandlerParam(..)
  , HandlerCheck
  , HandlerInfo(..)
  , LambdaCustomEpParam
  , LambdaDaoProposalMetadata(..)
  , LambdaExtra (..)
  , LambdaExtraRPC (..)
  , LambdaStorage
  , PhInput(..)
  , PhOutput(..)
  , ProposalHandler
  , RemoveHandlerParam
  ) where

import Universum hiding (fromInteger)

import Data.Map qualified as M

import Fmt (Buildable, build, genericF)
import Lorentz as L
import Lorentz.Lambda
import Morley.AsRPC

import Ligo.BaseDAO.RegistryDAO.Types
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

type HandlerStorage = Map MText ByteString

data PhInput = PhInput
  { piPacked_argument :: ByteString
  , piHandler_storage :: HandlerStorage
  , piProposal_info :: ProposeParams
  }

data PhOutput = PhOutput
  { poOperations :: [Operation]
  , poHandlerStorage :: HandlerStorage
  , poGuardian :: Maybe Address
  }

type HandlerCheck = Lambda (ByteString, HandlerStorage) ()

type ProposalHandler = Lambda PhInput PhOutput

data AddHandlerParam = AddHandlerParam
  { ahName :: MText
  , ahCode :: ProposalHandler
  , ahHandler_check :: HandlerCheck
  }

data HandlerInfo = HandlerInfo
  { _hiCode :: ProposalHandler
  , _hiHandler_check :: HandlerCheck
  , _hiIs_active :: Bool
  } deriving stock Eq

customGeneric "ExecuteHandlerParam" ligoLayout
deriving anyclass instance IsoValue ExecuteHandlerParam

customGeneric "AddHandlerParam" ligoLayout
customGeneric "PhInput" ligoLayout
deriving anyclass instance IsoValue PhInput

customGeneric "PhOutput" ligoLayout
deriving anyclass instance IsoValue PhOutput

data LambdaExtra = LambdaExtra
  { leLambdas :: BigMap MText HandlerInfo
  , leHandlerStorage :: HandlerStorage
  } deriving stock (Eq)

instance Default LambdaExtra where
  def = LambdaExtra (mkBigMap @(Map MText HandlerInfo) M.empty) mempty

type instance VariantToParam 'Lambda = ()
type instance VariantToExtra 'Lambda = LambdaExtra

type instance VariantToParam 'LambdaRegistry = RegistryCustomEpParam
type instance VariantToExtra 'LambdaRegistry = LambdaExtra
type instance VariantToParam 'LambdaTreasury = ()
type instance VariantToExtra 'LambdaTreasury = LambdaExtra

type LambdaStorage = StorageSkeleton (VariantToExtra 'Lambda)

type LambdaCustomEpParam = ()

instance HasAnnotation ExecuteHandlerParam where
  annOptions = baseDaoAnnOptions

instance HasAnnotation PhInput where
  annOptions = baseDaoAnnOptions

instance HasAnnotation PhOutput where
  annOptions = baseDaoAnnOptions

customGeneric "HandlerInfo" ligoLayout
deriving anyclass instance IsoValue HandlerInfo

instance HasAnnotation HandlerInfo where
  annOptions = baseDaoAnnOptions

deriving stock instance Generic (WrappedLambda i o)

instance Buildable (WrappedLambda i o) where
  build = genericF

instance Buildable HandlerInfo where
  build = genericF

customGeneric "LambdaExtra" ligoLayout
deriveRPCWithStrategy "LambdaExtra" ligoLayout
deriving anyclass instance IsoValue LambdaExtra

instance HasAnnotation LambdaExtra where
  annOptions = baseDaoAnnOptions

deriving anyclass instance IsoValue AddHandlerParam

instance HasAnnotation AddHandlerParam where
  annOptions = baseDaoAnnOptions

customGeneric "LambdaDaoProposalMetadata" ligoLayout
deriving anyclass instance IsoValue LambdaDaoProposalMetadata

instance HasAnnotation LambdaDaoProposalMetadata where
  annOptions = baseDaoAnnOptions

instance Buildable LambdaExtra where
  build = genericF
