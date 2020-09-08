-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-orphans #-}

-- | FA1.2 interface specification.

module Lorentz.Contracts.Spec.ApprovableLedgerInterface
  ( Parameter (..)
  , ParameterC
  , TransferParams
  , ApproveParams
  , GetAllowanceArg
  , GetBalanceArg
  , GetTotalSupplyArg
  , GetAllowanceParams
  , GetBalanceParams
  ) where

import Lorentz
import Lorentz.Contracts.Spec.AbstractLedgerInterface hiding (Parameter(..))

import Fmt (Buildable(..), (+|), (|+))

-- | Provides an FA1.2 interface that is supposed to be used by
-- applications that can work with any FA1.2-compatible contract.
-- This type should be transformed to a concrete contract
-- parameter and is not supposed be used in contract code, thus
-- it doesn't derive an `IsoValue`.
data Parameter
  = Transfer       TransferParams
  | Approve        ApproveParams
  | GetAllowance   GetAllowanceArg
  | GetBalance     GetBalanceArg
  | GetTotalSupply GetTotalSupplyArg
  deriving stock Generic
  deriving anyclass IsoValue

type ApproveParams = ("spender" :! Address, "value" :! Natural)
type GetAllowanceParams = ("owner" :! Address, "spender" :! Address)
type GetAllowanceArg = View GetAllowanceParams Natural

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

type ParameterC param =
  ParameterContainsEntrypoints param
    [ "Transfer" :> TransferParams
    , "Approve" :> ApproveParams
    , "GetAllowance" :> GetAllowanceArg
    , "GetBalance" :> GetBalanceArg
    , "GetTotalSupply" :> GetTotalSupplyArg
    ]

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | Attempt to change allowance from non-zero to a non-zero value.
type instance ErrorArg "unsafeAllowanceChange" = Natural

-- | Insufficient allowance to transfer foreign funds.
type instance ErrorArg "notEnoughAllowance" =
  ("required" :! Natural, "present" :! Natural)

----------------------------------------------------------------------------
-- Buildable instances
----------------------------------------------------------------------------

instance Buildable (CustomError "unsafeAllowanceChange") where
  build (CustomError _ prevVal) =
    "Cannot change allowance from " +| prevVal |+ " to a non-zero value"

instance Buildable (CustomError "notEnoughAllowance") where
  build (CustomError _ (arg #required -> required, arg #present -> present)) =
    "Insufficient allowance, you can spend only " +| present |+ ", but " +|
    required |+ " was requested"

----------------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------------

instance CustomErrorHasDoc "unsafeAllowanceChange" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Allowance change from non-zero value to non-zero value is performed. \
    \This contract does not allow such an update, see the [corresponding attack vector]\
    \(https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM) \
    \for explanation."
  customErrArgumentSemantics = Just
    "the previous value of approval"

instance CustomErrorHasDoc "notEnoughAllowance" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Not enough funds allowance to perform the operation."
