-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.Management
  ( acceptOwnership
  , transferOwnership
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.ManagedLedger.Doc (DRequireRole(..))

-- | Set the value of pending owner in storage
-- using the address in parameter.
transferOwnership
  :: Entrypoint TransferOwnershipParam Storage
transferOwnership = do
  dip authorizeAdmin

  -- Check if new owner is current admin. If so, we just store a `none`
  -- value to the pending owner field in the storage, and leave a False
  -- value on stack so that the rest of operation can be skipped. If not
  -- we just leave a True value to indicate that setting of new pending
  -- owner should continue.
  fromNamed #newOwner
  dup
  dip $ do
    dip $ stGetField #sAdmin

    if IsEq
      then do
        none
        stSetField #sPendingOwner
        push False
      else push True
  swap

  -- If the top value is True, we set the new pending owner. Or else
  -- we just drop the argument and be done with it.
  if_ (do
    some
    stSetField #sPendingOwner
    ) drop
  nil; pair

-- | Checks if pending owner is set and set the value of new administrator
acceptOwnership
  :: Entrypoint () Storage
acceptOwnership = do
  drop @()
  -- check if pending owner is set and put the unwrapped value of
  -- pending owner at the top. Throws error if pending owner is not set
  -- or sender address is different from the value of pending owner.
  authorizePendingOwner

  stSetField #sAdmin

  -- unset pending owner field in storage
  none
  stSetField #sPendingOwner
  nil; pair

-- Authorise sender and move pending owner address to stack top.
authorizePendingOwner ::
  StorageC store => store : s :-> Address : store : s
authorizePendingOwner = do
  doc $ DRequireRole "pending owner"
  stGetField #sPendingOwner
  if IsNone
    then failCustom_ #nO_PENDING_ADMINISTRATOR_SET
    else do
      dup
      dip $ do
        sender
        if IsEq then nop else failCustom_ #nOT_PENDING_ADMINISTRATOR

-- Authorise administrator.
authorizeAdmin ::
  StorageC store => store : s :-> store : s
authorizeAdmin = do
  doc $ DRequireRole "administrator"
  stGetField #sAdmin; sender;
  if IsEq then nop else failCustom_ #nOT_ADMINISTRATOR
