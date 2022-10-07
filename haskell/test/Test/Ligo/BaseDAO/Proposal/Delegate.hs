-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.BaseDAO.Proposal.Delegate
  ( test_UpdateDelegates
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Util.Named
import Test.Cleveland
import Test.Tasty (TestTree, testGroup)
import Morley.Tezos.Address

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

test_UpdateDelegates :: TestTree
test_UpdateDelegates =
  testGroup "Update_delegates:"
    [ testScenario "add/remove a delgate" $ scenario $
        addRemoveDelegate (originateLigoDaoWithConfigDesc @'Base ())
    , testScenario "update multiple delegates" $ scenario $
        updateDelegates (originateLigoDaoWithConfigDesc @'Base ()) checkIfDelegateExists
    ]

addRemoveDelegate
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> m ()
addRemoveDelegate originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  startLevel <- getOriginationLevel dodDao
  let param enable = DelegateParam
        { dpEnable = enable
        , dpDelegate = MkAddress dodOperator1
        }

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Freeze") (#amount :! 100)

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Update_delegate") [param True]

  advanceToLevel (startLevel + dodPeriod)
  let params counter = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer counter
        , ppFrom = MkAddress dodOwner1
        }
      key = makeProposalKey (params 1)
      vote = NoPermit VoteParam
                { vVoteType = True
                , vVoteAmount = 1
                , vProposalKey = key
                , vFrom = MkAddress dodOwner1
                }
  withSender dodOperator1 $ transfer dodDao $ calling (ep @"Propose") (params 1)
  advanceToLevel (startLevel + 2*dodPeriod)
  withSender dodOperator1 $ transfer dodDao $ calling (ep @"Vote") [vote]

  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Update_delegate") [param False]

  withSender dodOperator1 $ (transfer dodDao $ calling (ep @"Vote") [vote])
    & expectFailedWith notDelegate
  advanceToLevel (startLevel + 3*dodPeriod)
  withSender dodOperator1 $ (transfer dodDao $ calling (ep @"Propose") (params 2))
    & expectFailedWith notDelegate

updateDelegates
  :: (MonadCleveland caps m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn 'Base m)
  -> (forall p s. ContractHandle p s () -> Delegate -> m Bool)
  -> m ()
updateDelegates originateFn isDelegateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let mkDelegate addr enable = DelegateParam
        { dpEnable = enable
        , dpDelegate = addr
        }

  -- Before Delegate set
  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress dodOperator1)) @@== False
  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress dodOperator2)) @@== False

  -- Add a Delegate
  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Update_delegate") [mkDelegate (MkAddress dodOperator1) True]

  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress dodOperator1)) @@== True
  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress dodOperator2)) @@== False

  -- Add a new Delegate, remove an old one
  withSender dodOwner1
    $ transfer dodDao $ calling (ep @"Update_delegate")
      [mkDelegate (MkAddress dodOperator1) False, mkDelegate (MkAddress dodOperator2) True]

  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress dodOperator1)) @@== False
  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress dodOperator2)) @@== True


  -- Removing a non-existent delegate is allowed
  someAddr <- newAddress "some_address"
  withSender dodOwner1 $
    transfer dodDao $ calling (ep @"Update_delegate") [mkDelegate (MkAddress someAddr) False]

  isDelegateFn dodDao (Delegate (MkAddress dodOwner1) (MkAddress someAddr)) @@== False
