-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Common.Errors
  ( zeroMutezErrMsg
  , tooSmallXtzErrMsg
  , tooLargeXtzErrMsg
  , incorrectTokenAmountErrMsg
  , tooLargeProposalErrMsg
  ) where

import Lorentz

-- Error Msg used in Registry and Treasury DAO

zeroMutezErrMsg :: MText
zeroMutezErrMsg = [mt|ZERO_MUTEZ|]

tooSmallXtzErrMsg :: MText
tooSmallXtzErrMsg = [mt|LOW_XTZ|]

tooLargeXtzErrMsg :: MText
tooLargeXtzErrMsg = [mt|HIGH_XTZ|]

incorrectTokenAmountErrMsg :: MText
incorrectTokenAmountErrMsg = [mt|WRONG_TOKEN_AMOUNT|]

tooLargeProposalErrMsg :: MText
tooLargeProposalErrMsg = [mt|LARGE_PROPOSAL|]
