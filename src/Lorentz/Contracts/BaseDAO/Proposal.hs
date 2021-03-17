-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Proposal related entrypoints
module Lorentz.Contracts.BaseDAO.Proposal
  ( propose
  , vote
  , setVotingPeriod
  , setQuorumThreshold
  , flush
  , dropProposal
  , toProposalKey
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Doc
  (dropProposalDoc, flushDoc, proposeDoc, setQuorumThresholdDoc, setVotingPeriodDoc, voteDoc)
import Lorentz.Contracts.BaseDAO.Management (authorizeAdmin, ensureNotMigrated)
import Lorentz.Contracts.BaseDAO.Permit
import Lorentz.Contracts.BaseDAO.Token.FA2 (creditTo, debitFrom)
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface (TokenId)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

toProposalKey
  :: forall pm s. NicePackedValue pm
  => (ProposeParams pm, Address) : s :-> ProposalKey pm : s
toProposalKey = do
  pack @(ProposeParams pm, Address)
  blake2B

checkIfProposalExist
  :: forall store ce pm s. (KnownValue pm, StorageC store ce pm)
  => (ProposalKey pm : store : s) :-> (Proposal pm : s)
checkIfProposalExist = do
  stGet #sProposals
  ifSome nop (failCustomNoArg #pROPOSAL_NOT_EXIST)

ensureVotingPeriodIsNotOver
  :: forall store ce pm s. (KnownValue pm, StorageC store ce pm)
  => (Proposal pm : store : s) :-> s
ensureVotingPeriodIsNotOver = do
  toField #pStartDate
  swap
  stToField #sVotingPeriod; int
  add -- add to the start_date
  toNamed #expectedEndDate
  now; toNamed #currentDate
  if #currentDate >=. #expectedEndDate then
    failCustomNoArg #vOTING_PERIOD_OVER
  else
    nop

ensureProposalIsUnique
  :: forall store ce pm s. (NicePackedValue pm, StorageC store ce pm)
  => ProposeParams pm : store : s :-> ProposeParams pm : store : s
ensureProposalIsUnique = do
  dupTop2
  sender; swap; pair; toProposalKey
  stMem #sProposals
  if Holds then
    failCustomNoArg #pROPOSAL_NOT_UNIQUE
  else nop

------------------------------------------------------------------------
-- Propose
------------------------------------------------------------------------
checkIsProposalValid
  :: forall store ce pm op s. (StorageC store ce pm)
  => Config ce pm op -> (ProposeParams pm : store : s) :-> (ProposeParams pm : store : s)
checkIsProposalValid Config{..} = do
  dupTop2
  cProposalCheck
  stackType @(Bool : ProposeParams pm : store : s)
  if Holds then
    nop
  else
    failCustomNoArg #fAIL_PROPOSAL_CHECK

checkProposerUnfrozenToken
  :: forall store ce pm. (StorageC store ce pm)
  => '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
checkProposerUnfrozenToken = do
  duupX @2
  push unfrozenTokenId
  sender
  pair

  stGet #sLedger; ifSome nop $ do
    constructT @("required" :! Natural, "present" :! Natural)
      ( fieldCtor $ getField #ppFrozenToken >> toNamed #required
      , fieldCtor $ push (0 :: Natural) >> toNamed #present
      )
    failCustom #fA2_INSUFFICIENT_BALANCE

  toNamed #current_balance
  stackType @["current_balance" :! Natural, ProposeParams pm, store]
  duupX @2
  toFieldNamed #ppFrozenToken
  if #ppFrozenToken >. #current_balance then
    failCustomNoArg #pROPOSAL_INSUFFICIENT_BALANCE
  else do
    nop

checkProposalLimitReached
  :: forall store ce pm op. (StorageC store ce pm)
  => Config ce pm op -> '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
checkProposalLimitReached Config{..} = do
  duupX @2
  stToField #sProposalKeyListSortByDate
  size
  stackType @[Natural, ProposeParams pm, store]
  toNamed #currentAmount
  push (cMaxProposals); toNamed #maxAmount
  if #maxAmount <=. #currentAmount then
    failCustomNoArg #mAX_PROPOSALS_REACHED
  else nop

-- | Freeze the account's unfrozen token associated with the address
freeze
  :: forall store ce pm s. (StorageC store ce pm, HasFuncContext s store)
  => Natural : Address : store : s :-> store : s
freeze = do
  dig @2
  duupX @3
  duupX @3
  stackType @(Natural : Address : store : Natural : Address : s)

  push unfrozenTokenId
  pair
  dip swap; swap; dip swap
  stackType @(store : Address : (TokenId, Natural) : Natural : Address : s)
  callCachedFunc debitFrom

  stackType @(store : Natural : Address : s)
  dip $ do
    push frozenTokenId
    pair
    swap

  stackType @(store : Address : (TokenId, Natural) : s)
  callCachedFunc creditTo

-- | Unfreeze the account's frozen token associated with the address
unfreeze
  :: forall store ce pm s. (StorageC store ce pm, HasFuncContext s store)
  => Natural : Address : store : s :-> store : s
unfreeze = do
  dig @2
  duupX @3
  duupX @3
  stackType @(Natural : Address : store : Natural : Address : s)

  push frozenTokenId
  pair
  dip swap; swap; dip swap
  stackType @(store : Address : (TokenId, Natural) : Natural : Address : s)
  callCachedFunc debitFrom

  stackType @(store : Natural : Address : s)
  dip $ do
    push unfrozenTokenId
    pair
    swap

  stackType @(store : Address : (TokenId, Natural) : s)
  callCachedFunc creditTo

addProposal
  :: forall store ce pm. (NicePackedValue pm, StorageC store ce pm)
  => '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
addProposal = do
  ensureProposalIsUnique
  now; pair -- pStartDate
  constructT @(Proposal pm)
    ( fieldCtor $ push (0 :: Natural) -- initial upvote
    , fieldCtor $ push (0 :: Natural) -- initial downvote
    , fieldCtor $ do dup; car
    , fieldCtor $ do dup; cdr; toField #ppProposalMetadata
    , fieldCtor $ sender
    , fieldCtor $ do dup; cdr; toField #ppFrozenToken
    , fieldCtor $ push []
    )
  stackType @[Proposal pm, (Timestamp, ProposeParams pm), store]
  dip swap
  some
  duupX @3; cdr; sender; swap; pair; toProposalKey
  stackType @[ProposalKey pm, Maybe (Proposal pm), store, (Timestamp, ProposeParams pm)]
  dup; dip $ stUpdate #sProposals
  stackType @[ProposalKey pm, store, (Timestamp, ProposeParams pm)]
  dip $ stGetField #sProposalKeyListSortByDate -- Assuming passed proposal start immediately
  duupX @4; car; pair; -- create (timestamp, propsalKey)
  push True; swap; update
  stSetField #sProposalKeyListSortByDate
  stackType @[store, (Timestamp, ProposeParams pm)]
  swap; cdr

propose
  :: forall store ce pm op s.
     (StorageC store ce pm, NicePackedValue pm, HasFuncContext s store)
  => Config ce pm op -> Entrypoint' (ProposeParams pm) store s
propose config = do
  doc $ DDescription proposeDoc
  stackType @(ProposeParams pm : store : s)
  checkIsProposalValid config
  framed $ checkProposalLimitReached config
  framed $ checkProposerUnfrozenToken
  stackType @(ProposeParams pm : store : s)

  getField #ppFrozenToken; dip swap
  sender; swap
  freeze; swap
  stackType @(ProposeParams pm : store : s)

  framed addProposal
  drop @(ProposeParams pm)
  nil; pair

------------------------------------------------------------------------
-- Vote
------------------------------------------------------------------------

checkVoterUnfrozenToken
  :: forall store ce pm s.
     (StorageC store ce pm, HasNamedVar s "author" Address, VarIsUnnamed store)
   => VoteParam pm : store : s :-> VoteParam pm : store : s
checkVoterUnfrozenToken = do
  duupX @2
  push unfrozenTokenId; dupL #author; pair

  stGet #sLedger; ifSome nop
    ( do
        constructT @("required" :! Natural, "present" :! Natural)
          ( fieldCtor $ getField #vVoteAmount >> toNamed #required
          , fieldCtor $ push (0 :: Natural) >> toNamed #present
          )
        failCustom #fA2_INSUFFICIENT_BALANCE
    )

  toNamed #currentBalance
  stackType @("currentBalance" :! Natural : VoteParam pm : store : s)
  duupX @2
  toFieldNamed #vVoteAmount
  if #vVoteAmount >. #currentBalance then
    failCustomNoArg #vOTING_INSUFFICIENT_BALANCE
  else do
    nop

submitVote
  :: forall store ce pm s.
     (KnownValue pm, StorageC store ce pm, HasFuncContext s store)
  => VoteParam pm : store : "author" :! Address : s :-> store : s
submitVote = do
  dupTop2
  toField #vProposalKey
  checkIfProposalExist
  stackType @(Proposal pm : VoteParam pm : store : "author" :! _ : s)

  dig @2
  duupX @3; toField #vVoteAmount; dupL #author; swap
  freeze; dig @2; dig @2
  stackType @(Proposal pm : VoteParam pm : store : "author" :! _ : s)

  -- Update upvotes/downvotes
  duupX @2;
  dup @(VoteParam pm);
  dip $ toField #vVoteAmount
  toField #vVoteType
  if Holds then do
    stackType @(Natural : Proposal pm : VoteParam pm : store : "author" :! _ : s)
    dip $ getField #pUpvotes
    add
    setField #pUpvotes
  else do
    dip $ getField #pDownvotes
    add
    setField #pDownvotes

  stackType @(Proposal pm : VoteParam pm : store : "author" :! _ : s)

  duupX @2
  constructT @Voter
    ( fieldCtor $ getField #vVoteAmount
    , fieldCtor $ getField #vVoteType
    , fieldCtor $ dupL #author
    )
  dip $ do drop; getField #pVoters
  stackType @(Voter : [Voter] : Proposal pm : VoteParam pm : store : "author" :! _ : s)
  cons
  setField #pVoters

  stackType @(Proposal pm : VoteParam pm : store : "author" :! _ : s)

  swap; toField #vProposalKey
  stackType @(ProposalKey pm : Proposal pm : store : "author" :! _ : s)
  stInsert #sProposals
  dip $ drop @("author" :! _)

checkVoteLimitReached
  :: forall store ce pm op. (StorageC store ce pm)
  => Config ce pm op -> '[Proposal pm, VoteParam pm, store] :-> '[Proposal pm, VoteParam pm, store]
checkVoteLimitReached Config{..} = do
  dupTop2

  getField #pUpvotes; dip (toField #pDownvotes); add
  dip $ toField #vVoteAmount
  add; toNamed #newAmount

  push cMaxVotes; toNamed #maxAmount
  if #maxAmount <. #newAmount then
    failCustomNoArg #mAX_VOTES_REACHED
  else nop

-- | Vote
vote
  :: forall store ce pm op s.
     (StorageC store ce pm, HasFuncContext s store)
  => Config ce pm op -> Entrypoint' [PermitProtected $ VoteParam pm] store s
vote config = do
  doc $ DDescription voteDoc
  dip ensureNotMigrated
  iter $ do
    verifyPermitProtected #sPermitsCounter
    dip swap

    stackType @(VoteParam pm : store : "author" :! Address : s)

    dupTop2
    stackType @(VoteParam pm : store : VoteParam pm : store : "author" :! _ : s)

    toField #vProposalKey

    checkIfProposalExist
    stackType @(Proposal pm : VoteParam pm : store : "author" :! _ : s)

    framed $ checkVoteLimitReached config
    stackType @(Proposal pm : VoteParam pm : store : "author" :! _ : s)

    duupX @3
    swap
    stackType @(Proposal pm : store : VoteParam pm : store : "author" :! _ : s)

    ensureVotingPeriodIsNotOver
    stackType @(VoteParam pm : store : "author" :! _ : s)

    checkVoterUnfrozenToken
    stackType @(VoteParam pm : store : "author" :! _ : s)

    submitVote

  nil; pair

------------------------------------------------------------------------
-- Admin Entrypoints (set votingPeriod, quorum, flush)
------------------------------------------------------------------------

-- | Update voting period of all ongoing and new proposals.
setVotingPeriod
  :: forall store ce pm op s. (StorageC store ce pm)
  => Config ce pm op -> Entrypoint' VotingPeriod store s
setVotingPeriod Config{..}= do
  doc $ DDescription setVotingPeriodDoc
  dip $ do
    ensureNotMigrated
    authorizeAdmin

  toNamed #newValue
  dup
  push cMaxVotingPeriod; toNamed #maxValue
  if #maxValue <. #newValue then
    failCustomNoArg #oUT_OF_BOUND_VOTING_PERIOD
  else do
    dup
    push cMinVotingPeriod; toNamed #minValue
    if #minValue >. #newValue then
      failCustomNoArg #oUT_OF_BOUND_VOTING_PERIOD
    else do
      fromNamed #newValue
      stSetField #sVotingPeriod
      nil; pair

-- | Update quroum_threshold. The new quorum_threshold affects
-- all ongoing and new proposals.
setQuorumThreshold
  :: forall store ce pm op s. (StorageC store ce pm)
  => Config ce pm op -> Entrypoint' QuorumThreshold store s
setQuorumThreshold Config{..} = do
  doc $ DDescription setQuorumThresholdDoc
  dip $ do
    ensureNotMigrated
    authorizeAdmin

  toNamed #newValue
  dup
  push cMaxQuorumThreshold; toNamed #maxValue
  if #maxValue <. #newValue then
    failCustomNoArg #oUT_OF_BOUND_QUORUM_THRESHOLD
  else do
    dup
    push cMinQuorumThreshold; toNamed #minValue
    if #minValue >. #newValue then
      failCustomNoArg #oUT_OF_BOUND_QUORUM_THRESHOLD
    else do
      fromNamed #newValue
      stSetField #sQuorumThreshold
      nil; pair

-- Used in "flush". When unfreezing the tokens of voters/proposers
-- It is possible that the currentBalance is less than the frozenValue due to
-- the fact that admin can burn/transfer frozen token.
-- This function ensures that the "unfreeze" function will use the correct amount
-- to unfreeze.
checkBalanceLessThanFrozenValue :: forall store ce pm. (StorageC store ce pm)
  => [Natural, Address, store, Proposal pm, ProposalKey pm, [Operation]]
  :-> [Natural, Address, store, Proposal pm, ProposalKey pm, [Operation]]
checkBalanceLessThanFrozenValue = do
  toNamed #unfreeze_value
  swap; dip swap
  stackType @[Address, store, "unfreeze_value" :! Natural, Proposal pm, ProposalKey pm, [Operation]]
  dupTop2
  push frozenTokenId; swap; pair

  stGet #sLedger; ifSome nop $ do
    failCustomNoArg #pROPOSER_NOT_EXIST_IN_LEDGER

  stackType @[Natural, Address, store, "unfreeze_value" :! Natural, Proposal pm, ProposalKey pm, [Operation]]
  toNamed #actual_frozen_value
  dig @3
  stackType @["unfreeze_value" :! Natural, "actual_frozen_value" :! Natural, Address, store, Proposal pm, ProposalKey pm, [Operation]]

  dupTop2
  if #unfreeze_value >. #actual_frozen_value then do
    drop; fromNamed #actual_frozen_value
  else do
    dip drop; fromNamed #unfreeze_value

burnFrozenToken
  :: forall store ce pm s. (StorageC store ce pm, HasFuncContext s store)
  => (Address : Natural : store : s)
  :-> store : s
burnFrozenToken = do
  dip $ do push frozenTokenId; pair
  dig @2
  callCachedFunc debitFrom

-- | Burn the 'slash_amount' calculated by 'cRejectedProposalReturnValue'
-- If the 'slash_amount' is equal or bigger than the 'frozen' amount for that
-- particular proposal, we only burn value equal to the 'frozen' amount.
-- This is to avoid 'slash_amount' being abitrary large value, and we could burn
-- the whole proposer frozen balance.
burnSlashAmount
  :: forall store ce pm s. (StorageC store ce pm, HasFuncContext s store)
  => ("slash_amount" :! Natural) : ("frozen_tokens" :! Natural) : Address : store : s
  :-> ("slash_amount" :! Natural) : ("frozen_tokens" :! Natural) : Address : store : s
burnSlashAmount = do
  dupTop2
  swap; dip (fromNamed #slash_amount); fromNamed #frozen_tokens
  sub; isNat
  ifSome
    (do
      drop
      stackType @("slash_amount" :! Natural : "frozen_tokens" :! Natural : Address : store : s)
      dig @3; duupX @2; fromNamed #slash_amount -- Use the slash value
      duupX @5
      stackType @(Address : Natural : store : "slash_amount" :! Natural : "frozen_tokens" :! Natural : Address : s)
    )
    (do
      stackType @("slash_amount" :! Natural : "frozen_tokens" :! Natural : Address : store : s)
      dig @3; duupX @3; fromNamed #frozen_tokens -- Use the frozen balance value
      duupX @5
      stackType @(Address : Natural : store : "slash_amount" :! Natural : "frozen_tokens" :! Natural : Address : s)
    )
  burnFrozenToken
  stackType @(store : "slash_amount" :! Natural : "frozen_tokens" :! Natural : Address : s)
  swap; dip $ do swap; dip swap

unfreezeProposerToken
  :: forall store ce pm op s.
     (StorageC store ce pm, HasFuncContext s store)
  => Config ce pm op -> Bool -> Proposal pm : store : ProposalKey pm : [Operation] : s
  :-> Proposal pm : store : ProposalKey pm : [Operation] : s
unfreezeProposerToken Config{..} isAccepted = do

  handleSlashed
  stackType @(Natural : Address : store : Proposal pm : ProposalKey pm : [Operation] : s)

  framed checkBalanceLessThanFrozenValue
  unfreeze

  stackType @(store : Proposal pm : ProposalKey pm : [Operation] : s)
  swap
  where
    handleSlashed =
      case isAccepted of
        True -> do
          stackType @(Proposal pm : store : ProposalKey pm : [Operation] : s)
          getField #pProposerFrozenToken
          dip $ getField #pProposer
          dip $ dip swap
          stackType @(Natural : Address : store : Proposal pm : ProposalKey pm : [Operation] : s)

        False -> do
          dupTop2
          cRejectedProposalReturnValue
          stackType @("slash_amount" :! Natural : Proposal pm : store : ProposalKey pm : [Operation] : s)
          dip $ do
            getField #pProposerFrozenToken; toNamed #frozen_tokens
            dip $ getField #pProposer
            dip $ dip swap
          stackType @("slash_amount" :! Natural : "frozen_tokens" :! Natural : Address : store : Proposal pm : ProposalKey pm : [Operation] : s)
          burnSlashAmount

          -- Calculate unfreeze amount
          swap; dip (fromNamed #slash_amount); fromNamed #frozen_tokens
          sub; isNat
          ifSome nop $ do
            -- If the result is negative, we unfreeze 0 tokens.
            -- The frozen token balance associated with that proposal will be burn
            -- via 'burnSlashAmount'
            push (0 :: Natural)
            stackType @(Natural : Address : store : Proposal pm : ProposalKey pm : [Operation] : s)

unfreezeVoterToken
  :: forall store ce pm s.
     (StorageC store ce pm, HasFuncContext s store)
  => Proposal pm : store : ProposalKey pm : [Operation] : s
  :-> Proposal pm : store : ProposalKey pm : [Operation] : s
unfreezeVoterToken = do
  dup; dig @2; swap
  toField #pVoters
  iter $ do
    getField #voteAmount; dip (toField #voterAddress)
    unfreeze
  swap

isVotingPeriodOver
  :: forall store ce pm s.
     (StorageC store ce pm)
  => Proposal pm : store : s
  :-> Bool : s
isVotingPeriodOver = do
  toField #pStartDate
  swap; stToField #sVotingPeriod; int
  add -- add to the start_date
  toNamed #expectedEndDate
  now; toNamed #currentDate

  if #currentDate >=. #expectedEndDate then
    push True
  else
    push False

isCounterMet
  :: forall s. Counter : s
  :-> Bool : Counter : s
isCounterMet = do
  unpair
  swap; dup
  stackType @(Natural : Natural : "currentCount" :! Natural : s)
  toNamed #counter
  stackType @("counter" :! Natural : Natural : "currentCount" :! Natural : s)
  duupX @3
  if #currentCount >=. #counter  then do -- currentCount should never be bigger than counter
    swap; pair
    push True
  else do
    -- increment
    swap
    fromNamed #currentCount; push (1 :: Natural); add; toNamed #currentCount
    pair
    push False

doTotalVoteMeetQuorumThreshold
  :: forall store ce pm s.
     (StorageC store ce pm)
  => Proposal pm : store : s
  :-> Bool : s
doTotalVoteMeetQuorumThreshold = do
  getField #pUpvotes
  dip $ toField #pDownvotes
  add; toNamed #proposalTotalVote

  swap
  stToField #sQuorumThreshold; toNamed #sQuorumThreshold
  if #sQuorumThreshold <=. #proposalTotalVote then
    push True
  else
    push False

-- | Delete a proposal from 'sProposalKeyListSortByDate'
deleteProposal
  :: forall store ce pm s.
    (StorageC store ce pm)
  => Timestamp : ProposalKey pm : store : s
  :-> store : s
deleteProposal = do
  -- Delete from 'sProposalKeyListSortByDate'
  pair
  dip $ stGetField #sProposalKeyListSortByDate
  dip $ push False
  update
  stSetField #sProposalKeyListSortByDate

handleProposalIsOver
  :: forall store ce pm op s.
     (StorageC store ce pm, HasFuncContext s store)
  => Config ce pm op
  -> (Timestamp, ProposalKey pm) : store : [Operation] : Counter : s
  :-> store : [Operation] : Counter : s
handleProposalIsOver config@Config{..} = do
  cdr; dupTop2; checkIfProposalExist
  stackType @(Proposal pm : ProposalKey pm : store : [Operation] : Counter : s)
  dip swap
  dupTop2; isVotingPeriodOver
  if Holds then do
    dig @4
    isCounterMet; not
    if Holds then do
      dug @4
      dupTop2; doTotalVoteMeetQuorumThreshold
      dip $ do
        getFieldNamed #pUpvotes
        dip $ getFieldNamed #pDownvotes
        if #pUpvotes >. #pDownvotes
          then push True else push False
      and
      if Holds then do
        stackType @(Proposal pm : store : ProposalKey pm : [Operation] : Counter : s)
        unfreezeProposerToken config True
        unfreezeVoterToken

        dup; dip swap
        cDecisionLambda

        stackType @([Operation] : store : Proposal pm : ProposalKey pm : [Operation] : Counter : s)
        dig @4
        iter cons; swap
        stackType @(store : [Operation] : Proposal pm : ProposalKey pm : Counter: s)
        dig @3; dig @3
        toField #pStartDate; deleteProposal
      else do
        -- Reject proposal if (upvote <= downvote) or quorum threshold is not met
        stackType @(Proposal pm : store : ProposalKey pm : [Operation] : Counter : s)
        unfreezeProposerToken config False
        unfreezeVoterToken
        dip swap; toField #pStartDate; deleteProposal
    else do
      -- if counter is meet, we don't do anything
      dug @4
      drop; dip drop
  else do
    -- Do nothing
    drop; dip drop

-- | Flush all proposals that passed their voting period
-- If the proposal's total vote >= quorum_theshold and it's upvote > downvote
-- the proposal is accepted.
-- Otherwise it will be rejected.
flush
  :: forall store ce pm op s.
     (StorageC store ce pm, HasFuncContext s store)
  => Config ce pm op -> Entrypoint' Natural store s
flush config = do
  doc $ DDescription flushDoc

  -- guards
  dip $ do
    ensureNotMigrated

  ensureNot (0 :: Natural)

  push 0; toNamed #currentCount; pair
  nil
  dig @2; stGetField #sProposalKeyListSortByDate
  iter (handleProposalIsOver config)
  dip $ dip $ drop @Counter
  swap; pair

-- | 'dropProposal' accept a proposal key. It will remove a finished proposal from
-- `sProposalKeyListSortByDate`. This is mean to be used when `flush` get stuck on
-- a proposal that has fail `decisionLambda`. In other word, it is alike `flush` a
-- proposal without executing decision lambda.
dropProposal
  :: forall store ce pm op s. (StorageC store ce pm, HasFuncContext s store)
  => Config ce pm op -> Entrypoint' (ProposalKey pm) store s
dropProposal config = do
  doc $ DDescription dropProposalDoc

  -- guards
  dip $ do
    ensureNotMigrated
    authorizeAdmin

  dupTop2; checkIfProposalExist
  stackType @(Proposal pm : ProposalKey pm : store : s)
  dip swap
  dupTop2; isVotingPeriodOver
  if Holds then do
    dupTop2; doTotalVoteMeetQuorumThreshold
    dip $ do
      getFieldNamed #pUpvotes
      dip $ getFieldNamed #pDownvotes
      if #pUpvotes >. #pDownvotes
        then push True else push False
    and
    if Holds then do
      nil; dug @3
      stackType @(Proposal pm : store : ProposalKey pm : [Operation] : s)
      unfreezeProposerToken config True
      unfreezeVoterToken
      stackType @(Proposal pm : store : ProposalKey pm : [Operation] : s)
      dip swap
      toField #pStartDate; deleteProposal
      swap; pair
    else do
      failCustomNoArg #fAIL_DROP_PROPOSAL_NOT_ACCEPTED
  else do
    failCustomNoArg #fAIL_DROP_PROPOSAL_NOT_OVER

ensureNot :: forall a s.
  ( NiceParameter a, NiceStorage a
  , NicePackedValue a, NiceComparable a
  ) => a -> (a : s) :-> (a : s)
ensureNot input = do
  dup; toNamed #val
  push input; toNamed #input
  if #input ==. #val then
    failCustomNoArg #bAD_ENTRYPOINT_PARAMETER
  else
    nop
