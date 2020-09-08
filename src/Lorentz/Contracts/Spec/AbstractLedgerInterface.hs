-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-orphans #-}

-- | AbstractLedger interface specification.

module Lorentz.Contracts.Spec.AbstractLedgerInterface
  ( Parameter (..)
  , TransferParams
  , GetBalanceArg
  , GetTotalSupplyArg
  , GetBalanceParams
  ) where

import Lorentz

import Fmt (Buildable(..), (+|), (|+))

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

data Parameter
  = Transfer       TransferParams
  | GetTotalSupply GetTotalSupplyArg
  | GetBalance     GetBalanceArg
  deriving stock Generic
  deriving anyclass IsoValue

type TransferParams = ("from" :! Address, "to" :! Address, "value" :! Natural)
type GetBalanceParams = ("owner" :! Address)

type GetBalanceArg = View GetBalanceParams Natural
type GetTotalSupplyArg = View () Natural

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | Insufficient balance.
type instance ErrorArg "notEnoughBalance" =
  ("required" :! Natural, "present" :! Natural)

-- | In this abstract ledger, we only accept :from == sender. Other standards
-- may allow different kinds of transfer, e.g. check whether sender is
-- approved by :from.
type instance ErrorArg "nonAcceptableSource" =
  ("sender" :! Address, "from" :! Address)

----------------------------------------------------------------------------
-- Buildable instances
----------------------------------------------------------------------------

instance Buildable (CustomError "notEnoughBalance") where
  build (CustomError _ (arg #required -> required, arg #present -> present)) =
    "Insufficient balance, needed " +| required |+ ", but only" +|
    present |+ " is present"

instance Buildable (CustomError "nonAcceptableSource") where
  build (CustomError _ (arg #sender -> senderAddr, arg #from -> from)) =
    "Non acceptable source reached, from " +| from |+ ", with actual sender" +|
    senderAddr |+ " provided."

----------------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------------

instance CustomErrorHasDoc "notEnoughBalance" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Not enough funds to perform the operation."

instance CustomErrorHasDoc "nonAcceptableSource" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Non acceptable source for transaction provided."
