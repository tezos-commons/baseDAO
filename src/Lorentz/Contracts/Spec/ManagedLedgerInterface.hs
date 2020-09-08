-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Managed ledger which is compatible with FA1.2 standard and
-- extended with administrator functionality.

module Lorentz.Contracts.Spec.ManagedLedgerInterface
  ( -- * Parameter
    Parameter (..)
  , ParameterC
  , AL.TransferParams
  , AL.ApproveParams
  , AL.GetAllowanceArg
  , AL.GetBalanceArg
  , AL.GetTotalSupplyArg
  , AL.GetAllowanceParams
  , AL.GetBalanceParams
  , ApproveCasParams
  , AllowanceParams
  , MintParams
  , BurnParams
  ) where

import Lorentz

import Fmt (Buildable(..), (+|), (|+))

import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

type ApproveCasParams =
  ("spender" :! Address, "value" :! Natural, "expected" :! Natural)

type AllowanceParams =
  ("owner" :! Address, "spender" :! Address, "value" :! Natural)

type MintParams =
  ("to" :! Address, "value" :! Natural)

type BurnParams =
  ("from" :! Address, "value" :! Natural)

data Parameter
  = Transfer         AL.TransferParams
  | Approve          AL.ApproveParams
  | ApproveCAS       ApproveCasParams
  | GetAllowance     AL.GetAllowanceArg
  | GetBalance       AL.GetBalanceArg
  | GetTotalSupply   AL.GetTotalSupplyArg
  | SetPause         Bool
  | SetAdministrator Address
  | GetAdministrator (View () Address)
  | Mint             MintParams
  | Burn             BurnParams
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

type ParameterC param =
  ParameterContainsEntrypoints param
    [ "Transfer"         :> AL.TransferParams
    , "Approve"          :> AL.ApproveParams
    , "ApproveCAS"       :> ApproveCasParams
    , "GetAllowance"     :> AL.GetAllowanceArg
    , "GetBalance"       :> AL.GetBalanceArg
    , "GetTotalSupply"   :> AL.GetTotalSupplyArg
    , "SetPause"         :> Bool
    , "SetAdministrator" :> Address
    , "GetAdministrator" :> (View () Address)
    , "Mint"             :> MintParams
    , "Burn"             :> BurnParams
    ]

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | All kinds of transfers are unavailable until resumed by token admin.
type instance ErrorArg "tokenOperationsArePaused" = ()

-- | Updated allowance is not the same as expected implying
-- that tokens were used before changing the allowance.
type instance ErrorArg "allowanceMismatch" =
  ("actual" :! Natural, "expected" :! Natural)

-- Buildable instances
----------------------------------------------------------------------------

instance Buildable (CustomError "tokenOperationsArePaused") where
  build (CustomError _ ()) =
    "Operations are paused and cannot be invoked"

instance Buildable (CustomError "allowanceMismatch") where
  build (CustomError _ (actual, expected)) =
    "Expected allowance (" +| expected |+
    ") does not match the actual one (" +| actual |+ ")"

-- Documentation
----------------------------------------------------------------------------

instance CustomErrorHasDoc "tokenOperationsArePaused" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Token functionality (`transfer` and similar entrypoints) is suspended."

instance CustomErrorHasDoc "allowanceMismatch" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "Expected allowance does not match the actual one"
  customErrArgumentSemantics =
    Just "`(actual allowance, expected allowance)` pair"
