-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | `Model` refer to Haskell implementation of BaseDAO
module SMT.Model.BaseDAO.Contract
  ( handleCallViaHaskell
  ) where

import Universum hiding (drop, swap)

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Management
import SMT.Model.BaseDAO.Proposal
import SMT.Model.BaseDAO.Token
import SMT.Model.BaseDAO.Types

-- | Advance level of the blockchain then run haskell model against the call.
-- Return the new `ModelState` and error from the call if there is any.
handleCallViaHaskell :: ModelCall -> ModelState -> (Maybe ModelError, ModelState)
handleCallViaHaskell mc ms =
  case runModelT action ms' of
    Right updatedMs -> (Nothing, updatedMs)
    Left err -> (Just err, ms')

  where
    ms' = case mc & mcAdvanceLevel of
            Just lvl -> ms { msLevel = (ms & msLevel) + lvl}
            Nothing -> ms

    action =
      case mc & mcParameter of
        XtzAllowed (Propose p) -> applyPropose (mc & mcSource) p
        XtzAllowed (Transfer_contract_tokens p) -> applyTransferContractTokens (mc & mcSource) p
        XtzAllowed (Transfer_ownership p) -> applyTransferOwnership (mc & mcSource) p
        XtzAllowed (Accept_ownership _) -> applyAcceptOwnership (mc & mcSource)
        XtzAllowed (CallCustom p) -> applyCallCustom (mc & mcSource) p
        XtzForbidden (Vote p) -> applyVote (mc & mcSource) p
        XtzForbidden (Flush p) -> applyFlush (mc & mcSource) p
        XtzForbidden (Freeze p) -> applyFreeze (mc & mcSource) p
        XtzForbidden (Unfreeze p) -> applyUnfreeze (mc & mcSource) p
        XtzForbidden (Update_delegate p) -> applyUpdateDelegate (mc & mcSource) p
        XtzForbidden (Drop_proposal p) -> applyDropProposal (mc & mcSource) p
