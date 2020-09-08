-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Documentation for BaseDAO contract.

module Lorentz.Contracts.BaseDAO.Doc
  ( contractDoc
  -- , transferDoc
  -- , approveDoc
  -- , approveCASDoc
  -- , getAllowanceDoc
  -- , getBalanceDoc
  -- , getTotalSupplyDoc
  -- , setPauseDoc
  -- , setAdministratorDoc
  -- , getAdministratorDoc
  -- , mintDoc
  -- , burnDoc

  -- , DTokenNotPausedOnly (..)
  -- , DRequireRole (..)
  ) where

import Lorentz

import Fmt (build)

import Util.Markdown

contractDoc :: Markdown
contractDoc = [md|
  This documentation describes a smart contract which implements
  [FA1.2 interface](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-7/tzip-7.md).
  This contract also maintains an entity called _administrator_ which has an exclusive right to perform management operations like `Mint` and `Pause`.
  |]

-- transferDoc :: Markdown
-- transferDoc = [md|
--   Transfers tokens between two given accounts.

--   This entrypoint serves multiple purposes:
--   * When called with `"from"` account equal to the transaction sender, we assume that
--   the user transfers their own money and this does not require approval.
--   * Otherwise, the transaction sender must be previously authorized to transfer at least the requested number of tokens from the `"from"` account using the `approve` entrypoint.
--   In this case current number of tokens that sender is allowed to withdraw from the `"from"` address is decreased by the number of transferred tokens.

--   |]

-- approveDoc :: Markdown
-- approveDoc = [md|
--   When called with `(address :spender, nat :value)`
--   parameters allows `spender` account to withdraw from the sender, multiple times,
--   up to the `value` amount.
--   Each call of `transfer` entrypoint decreases the allowance amount on the transferred amount of
--   tokens, unless `transfer` is called with `from` account equal to sender, in which case allowance
--   is always ignored.
--   In other terms self-approval, where 'from` is equal to sender, is redundant and will never be consumed by a 'transfer'.

--   If this entrypoint is called again, it overwrites the current allowance with `value`.

--   **DISCLAIMER**: this suffers from an [attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM),
--   that is a known issue of `ERC20` and, as a consequence, of `FA1.2` (which is
--   based on it).
--   It is not safe to change the approval from a non-zero value to a non-zero value.
--   This is the reason why performing such a change directly is not allowed by the contract.
--   However this is not enough on its own, a token holder that intends to
--   safely change the allowance for `X` to `K` token must:
--   1. read the current allowance `M` for `X` from the latest transaction `S`.
--   2. send a transaction `T` that sets the allowance to `0`.
--   3. wait for the blockchain to confirm that `T` is included.
--   4. scan all transactions between `S` and `T`.
--   5. calculate the allowance `N <= M` spent by `X` in those transactions.
--   6. set the allowance to `K - N` iff `N < K`.
--   |]

-- approveCASDoc :: Markdown
-- approveCASDoc = [md|
--   Compare the expected allowance value with the actual one and set a new one if they match.

--   If the current amount of sender's tokens `spender` is allowed to spend is **not** equal to the `expected` value, this function fails with `allowanceMismatch` error.
--   Otherwise it behaves as `approve` and does not prohibit changing allowance from non-zero to non-zero.
--   |]

-- getAllowanceDoc :: Markdown
-- getAllowanceDoc =
--   "Returns the approval value between two given addresses."

-- getBalanceDoc :: Markdown
-- getBalanceDoc =
--   "Returns the balance of the address in the ledger."

-- getTotalSupplyDoc :: Markdown
-- getTotalSupplyDoc =
--   "Returns total number of tokens."

-- setPauseDoc :: Markdown
-- setPauseDoc = [md|
--   This entrypoint pauses operations when the parameter is `True`,
--   and resumes them when the parameter is `False`. During the pause,
--   no contract can perform `transfer` or `approval` operations.
--   |]

-- setAdministratorDoc :: Markdown
-- setAdministratorDoc =
--   "Change the current administrator."

-- getAdministratorDoc :: Markdown
-- getAdministratorDoc =
--   "This view returns the current administrator."

-- mintDoc :: Markdown
-- mintDoc =
--   "Produces tokens on the account associated with the given address."

-- burnDoc :: Markdown
-- burnDoc =
--   "Destroys the given amount of tokens on the account associated with \
--   \the given address."

-- -- | Mention that given action can be executed only when
-- -- transfer-like operations are not paused.
-- data DTokenNotPausedOnly = DTokenNotPausedOnly

-- instance DocItem DTokenNotPausedOnly where
--   docItemPos = 1235
--   docItemSectionName = Nothing
--   docItemToMarkdown _ DTokenNotPausedOnly =
--     mdSubsection "Pausable"
--                  "Cannot be executed when token operations are paused."

-- -- | Mention that entrypoint requires sender to have special privileges.
-- --
-- -- You provide textual description of these privileges, e.g. "administrator"
-- -- or "trusted".
-- data DRequireRole = DRequireRole Text

-- instance DocItem DRequireRole where
--   docItemPos = 1230
--   docItemSectionName = Nothing
--   docItemToMarkdown _ (DRequireRole role) =
--     mdSubsection "Authorization" $
--       "The sender has to be " <> mdTicked (build role) <> "."
