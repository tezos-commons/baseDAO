-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- NOTE: This file should not be modified directly.
-- Use @stack scripts/generate_error_code.hs@ instead.

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Ligo.BaseDAO.ErrorCodes where

import Universum

import Morley.Micheline (toExpression)
import Morley.Michelson.Text
import Morley.Michelson.Typed (toVal)
import Lorentz.Contracts.Spec.TZIP16Interface

----------------------------------------------------------------------------
-- Invalid Input Error Codes
----------------------------------------------------------------------------

-- | The sender is not the administrator.
notAdmin :: Natural
notAdmin = 100

-- | The sender is not the current pending administrator.
notPendingAdmin :: Natural
notPendingAdmin = 101

-- | Thrown paired with a `string` error message when the proposal does not pass the `proposal_check`.
failProposalCheck :: Natural
failProposalCheck = 102

-- | The proposal does not exist or is no longer ongoing.
proposalNotExist :: Natural
proposalNotExist = 103

-- | The proposal voting stage has already ended.
votingStageOver :: Natural
votingStageOver = 104

-- | Transfer of XTZ is forbidden on this entrypoint.
forbiddenXtz :: Natural
forbiddenXtz = 107

-- | The submitted proposal already exist.
proposalNotUnique :: Natural
proposalNotUnique = 108

-- | Parameter signature does not match the expected one - for permits.
missigned :: Natural
missigned = 109

-- | The unpacking of a submitted value failed.
unpackingFailed :: Natural
unpackingFailed = 110

-- | The unpacking of a proposal metadata failed.
unpackingProposalMetadataFailed :: Natural
unpackingProposalMetadataFailed = 111

-- | A required field value was not found.
missingValue :: Natural
missingValue = 112

-- | Proposals cannot be submitted in non-proposing stages.
notProposingStage :: Natural
notProposingStage = 113

-- | There aren't enough frozen tokens for the operation.
notEnoughFrozenTokens :: Natural
notEnoughFrozenTokens = 114

-- | The governance token contract does not have the expected entrypoints.
badTokenContract :: Natural
badTokenContract = 115

-- | The type of a contract for a view entrypoint is not of the expected type.
badViewContract :: Natural
badViewContract = 116

-- | The conditions to drop this proposal are not met.
dropProposalConditionNotMet :: Natural
dropProposalConditionNotMet = 117

-- | The proposal has expired and can no longer be flushed.
expiredProposal :: Natural
expiredProposal = 118

-- | There are no available proposals to flush.
emptyFlush :: Natural
emptyFlush = 119

-- | The sender has not been delegated the control of the required tokens.
notDelegate :: Natural
notDelegate = 120

-- | Executing the proposal's decision callback results in failure.
failDecisionCallback :: Natural
failDecisionCallback = 121

-- | Cannot call `unstake_vote` on the proposal that is not flushed or dropped.
unstakeInvalidProposal :: Natural
unstakeInvalidProposal = 123

-- | The sender did not vote on the proposal or already unstaked tokens from the proposal.
voterDoesNotExist :: Natural
voterDoesNotExist = 124






----------------------------------------------------------------------------
-- Internal Error Codes
----------------------------------------------------------------------------

-- | Throw when storage is in an unexpected state, indicating a contract error.
badState :: Natural
badState = 300




----------------------------------------------------------------------------
-- TZIP 16 error list
----------------------------------------------------------------------------
tzipErrorList :: [Error]
tzipErrorList = [
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger notAdmin
    , seExpansion = toExpression $ toVal @MText [mt|The sender is not the administrator.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger notPendingAdmin
    , seExpansion = toExpression $ toVal @MText [mt|The sender is not the current pending administrator.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @(Integer, MText) (toInteger failProposalCheck, [mt|<reason>|])
    , seExpansion = toExpression $ toVal @MText [mt|Thrown paired with a `string` error message when the proposal does not pass the `proposal_check`.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger proposalNotExist
    , seExpansion = toExpression $ toVal @MText [mt|The proposal does not exist or is no longer ongoing.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger votingStageOver
    , seExpansion = toExpression $ toVal @MText [mt|The proposal voting stage has already ended.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger forbiddenXtz
    , seExpansion = toExpression $ toVal @MText [mt|Transfer of XTZ is forbidden on this entrypoint.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger proposalNotUnique
    , seExpansion = toExpression $ toVal @MText [mt|The submitted proposal already exist.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger missigned
    , seExpansion = toExpression $ toVal @MText [mt|Parameter signature does not match the expected one - for permits.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @(Integer, MText) (toInteger unpackingFailed, [mt|<reason>|])
    , seExpansion = toExpression $ toVal @MText [mt|The unpacking of a submitted value failed.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger unpackingProposalMetadataFailed
    , seExpansion = toExpression $ toVal @MText [mt|The unpacking of a proposal metadata failed.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @(Integer, MText) (toInteger missingValue, [mt|<name of the field>|])
    , seExpansion = toExpression $ toVal @MText [mt|A required field value was not found.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger notProposingStage
    , seExpansion = toExpression $ toVal @MText [mt|Proposals cannot be submitted in non-proposing stages.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger notEnoughFrozenTokens
    , seExpansion = toExpression $ toVal @MText [mt|There aren't enough frozen tokens for the operation.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger badTokenContract
    , seExpansion = toExpression $ toVal @MText [mt|The governance token contract does not have the expected entrypoints.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger badViewContract
    , seExpansion = toExpression $ toVal @MText [mt|The type of a contract for a view entrypoint is not of the expected type.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger dropProposalConditionNotMet
    , seExpansion = toExpression $ toVal @MText [mt|The conditions to drop this proposal are not met.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger expiredProposal
    , seExpansion = toExpression $ toVal @MText [mt|The proposal has expired and can no longer be flushed.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger emptyFlush
    , seExpansion = toExpression $ toVal @MText [mt|There are no available proposals to flush.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger notDelegate
    , seExpansion = toExpression $ toVal @MText [mt|The sender has not been delegated the control of the required tokens.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger failDecisionCallback
    , seExpansion = toExpression $ toVal @MText [mt|Executing the proposal's decision callback results in failure.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger unstakeInvalidProposal
    , seExpansion = toExpression $ toVal @MText [mt|Cannot call `unstake_vote` on the proposal that is not flushed or dropped.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger voterDoesNotExist
    , seExpansion = toExpression $ toVal @MText [mt|The sender did not vote on the proposal or already unstaked tokens from the proposal.|]
    , seLanguages = ["en"]
    }
  ,
  EStatic $ StaticError
    { seError = toExpression $ toVal @Integer $ toInteger badState
    , seExpansion = toExpression $ toVal @MText [mt|Throw when storage is in an unexpected state, indicating a contract error.|]
    , seLanguages = ["en"]
    }
  ]

