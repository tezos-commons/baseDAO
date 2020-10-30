-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Proposal related entrypoints
module Lorentz.Contracts.BaseDAO.Proposal
  ( propose
  , proposalMetadata
  , vote
  , setVotingPeriod
  , setQuorumThreshold
  , flush
  ) where

import Lorentz
import Lorentz.Contracts.BaseDAO.Management (authorizeAdmin)
import Lorentz.Contracts.BaseDAO.FA2 (debitFrom, creditTo)
import Lorentz.Contracts.BaseDAO.Types
import Lorentz.Contracts.Spec.FA2Interface (TokenId)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

checkIfProposalExist
  :: forall store pm s. (NiceParameter pm, StorageC store pm)
  => (ProposalKey : store : s) :-> (Proposal pm : s)
checkIfProposalExist = do
  stGet #sProposals
  ifSome nop (failCustom_ #pROPOSAL_NOT_EXIST)

ensureVotingPeriodIsNotOver
  :: forall store pm s. (NiceParameter pm, StorageC store pm)
  => (Proposal pm : store : s) :-> s
ensureVotingPeriodIsNotOver = do
  toField #pStartDate
  swap
  stToField #sVotingPeriod; int
  add -- add to the start_date
  toNamed #expectedEndDate
  now; toNamed #currentDate
  if #currentDate >=. #expectedEndDate then
    failCustom_ #vOTING_PERIOD_OVER
  else
    nop

ensureProposalIsUnique
  :: forall store pm s. (NicePackedValue pm, StorageC store pm)
  => ProposeParams pm : store : s :-> ProposeParams pm : store : s
ensureProposalIsUnique = do
  dupTop2
  sender; swap; pair; toProposalKey
  swap; stToField #sProposalKeyListSortByDate
  stackType @([ByteString] : ByteString : ProposeParams pm : store : s)
  iter $ do
    dip dup
    if IsEq then
      failCustom_ #pROPOSAL_NOT_UNIQUE
    else
      nop
  drop

toProposalKey
  :: forall pm s. NicePackedValue pm
  => (ProposeParams pm, Address) : s :-> ByteString : s
toProposalKey = do
  pack @(ProposeParams pm, Address)
  blake2B

------------------------------------------------------------------------
-- Propose
------------------------------------------------------------------------

checkIsProposalValid
  :: forall store pm. (StorageC store pm)
  => Config pm -> '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
checkIsProposalValid Config{..} = do
  dup
  cProposalCheck
  stackType @[Bool, ProposeParams pm, store]
  if Holds then
    nop
  else
    failCustom_ #fAIL_PROPOSAL_CHECK

checkProposerUnfrozenToken
  :: forall store pm. (IsoValue pm, StorageC store pm)
  => '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
checkProposerUnfrozenToken = do
  duupX @2
  push unfrozenTokenId
  sender
  pair

  stGet #sLedger; ifSome nop $ do
    constructT
      ( fieldCtor $ getField #ppFrozenToken >> toNamed #required
      , fieldCtor $ push (0 :: Natural) >> toNamed #present
      )
    failCustom #fA2_INSUFFICIENT_BALANCE

  toNamed #current_balance
  stackType @["current_balance" :! Natural, ProposeParams pm, store]
  duupX @2
  toFieldNamed #ppFrozenToken
  if #ppFrozenToken >. #current_balance then
    failCustom_ #pROPOSAL_INSUFFICIENT_BALANCE
  else do
    nop

checkProposalLimitReached
  :: forall store pm. (StorageC store pm)
  => Config pm -> '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
checkProposalLimitReached Config{..} = do
  duupX @2
  stToField #sProposalKeyListSortByDate
  size
  stackType @[Natural, ProposeParams pm, store]
  toNamed #currentAmount
  push (cMaxProposals); toNamed #maxAmount
  if #maxAmount <=. #currentAmount then
    failCustom_ #mAX_PROPOSALS_REACHED
  else nop

-- | Freeze the account's unfrozen token associated with the address
freeze
  :: forall store pm s. (StorageC store pm)
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
  debitFrom

  stackType @(store : Natural : Address : s)
  dip $ do
    push frozenTokenId
    pair
    swap

  stackType @(store : Address : (TokenId, Natural) : s)
  creditTo

-- | Unfreeze the account's frozen token associated with the address
unfreeze
  :: forall store pm s. (StorageC store pm)
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
  debitFrom

  stackType @(store : Natural : Address : s)
  dip $ do
    push unfrozenTokenId
    pair
    swap

  stackType @(store : Address : (TokenId, Natural) : s)
  creditTo

addProposal
  :: forall store pm. (NicePackedValue pm, StorageC store pm)
  => '[ProposeParams pm, store] :-> '[ProposeParams pm, store]
addProposal = do
  ensureProposalIsUnique
  constructT @(Proposal pm)
    ( fieldCtor $ push (0 :: Natural) -- initial upvote
    , fieldCtor $ push (0 :: Natural) -- initial downvote
    , fieldCtor $ now -- pStartDate
    , fieldCtor $ getField #ppProposalMetadata
    , fieldCtor $ sender
    , fieldCtor $ getField #ppFrozenToken
    , fieldCtor $ push []
    )
  dip swap
  stackType @[Proposal pm, store, ProposeParams pm]
  some
  duupX @3; sender; swap; pair; toProposalKey
  stackType @[ProposalKey, Maybe (Proposal pm), store, ProposeParams pm]
  stUpdate #sProposals

  stGetField #sProposalKeyListSortByDate
  duupX @3; sender; swap; pair; toProposalKey
  stackType @[ProposalKey, [ProposalKey], store, ProposeParams pm]
  cons -- Assuming passed proposal start immediately
  stackType @[[ProposalKey], store, ProposeParams pm]
  stSetField #sProposalKeyListSortByDate
  swap

propose
  :: forall store pm. (StorageC store pm, NicePackedValue pm)
  => Config pm -> Entrypoint (ProposeParams pm) store
propose config = do
  stackType @[ProposeParams pm, store]
  checkIsProposalValid config
  checkProposalLimitReached config
  checkProposerUnfrozenToken
  stackType @[ProposeParams pm, store]

  getField #ppFrozenToken; dip swap
  sender; swap
  freeze; swap
  stackType @[ProposeParams pm, store]

  addProposal
  drop @(ProposeParams pm)
  nil; pair

------------------------------------------------------------------------
-- Vote
------------------------------------------------------------------------

checkVoterUnfrozenToken
  :: forall store pm. (StorageC store pm)
   => '[VoteParam, store] :-> '[VoteParam, store]
checkVoterUnfrozenToken = do
  duupX @2
  push unfrozenTokenId; sender; pair

  stGet #sLedger; ifSome nop
    ( do
        constructT
          ( fieldCtor $ getField #vVoteAmount >> toNamed #required
          , fieldCtor $ push (0 :: Natural) >> toNamed #present
          )
        failCustom #fA2_INSUFFICIENT_BALANCE
    )

  toNamed #currentBalance
  stackType @["currentBalance" :! Natural, VoteParam, store]
  duupX @2
  toFieldNamed #vVoteAmount
  if #vVoteAmount >. #currentBalance then
    failCustom_ #vOTING_INSUFFICIENT_BALANCE
  else do
    nop

submitVote
  :: forall store pm. (NiceParameter pm, StorageC store pm)
  => '[VoteParam, store] :-> '[store]
submitVote = do
  dupTop2
  toField #vProposalKey

  stGet #sProposals
  ifSome nop (failCustom_ #pROPOSAL_NOT_EXIST)
  stackType @[Proposal pm, VoteParam, store]

  dig @2
  duupX @3; toField #vVoteAmount; sender; swap
  freeze; dig @2; dig @2
  stackType @[Proposal pm, VoteParam, store]

  constructT @(Proposal pm)
    ( fieldCtor $ do
        -- | Update upvote
        duupX @2;
        stackType @[VoteParam, Proposal pm, VoteParam, store]
        toField #vVoteType;
        stackType @[Bool, Proposal pm, VoteParam, store]
        if Holds then do
          duupX @2
          stackType @[VoteParam, Proposal pm, VoteParam, store]
          toField #vVoteAmount
          duupX @2
          stackType @[Proposal pm, Natural, Proposal pm, VoteParam, store]
          toField #pUpvotes
          add
        else do
          stackType @[Proposal pm, VoteParam, store]
          getField #pUpvotes
    , fieldCtor $ do
        -- | Update downvote
        duupX @2
        stackType @[VoteParam, Proposal pm, VoteParam, store]
        toField #vVoteType;
        stackType @[Bool, Proposal pm, VoteParam, store]
        if Holds then do
          stackType @[Proposal pm, VoteParam, store]
          getField #pDownvotes
        else do
          duupX @2
          stackType @[VoteParam, Proposal pm, VoteParam, store]
          toField #vVoteAmount
          duupX @2
          stackType @[Proposal pm, Natural, Proposal pm, VoteParam, store]
          toField #pDownvotes
          add

    , fieldCtor $ getField #pStartDate
    , fieldCtor $ getField #pMetadata
    , fieldCtor $ getField #pProposer
    , fieldCtor $ getField #pProposerFrozenToken
    , fieldCtor $ do
        duupX @2; toField #vVoteAmount
        sender; pair
        dip $ getField #pVoters
        stackType @[(Address, Natural), [(Address, Natural)], Proposal pm, VoteParam, store]
        cons
    )

  stackType @[Proposal pm, Proposal pm, VoteParam, store]
  dip drop
  stackType @[Proposal pm, VoteParam, store]

  some
  swap; toField #vProposalKey
  stackType @[ProposalKey, Maybe (Proposal pm), store]
  stUpdate #sProposals

checkVoteLimitReached
  :: forall store pm. (IsoValue pm, StorageC store pm)
  => Config pm -> '[Proposal pm, VoteParam, store] :-> '[Proposal pm, VoteParam, store]
checkVoteLimitReached Config{..} = do
  dupTop2

  getField #pUpvotes; dip (toField #pDownvotes); add
  dip $ toField #vVoteAmount
  add; toNamed #newAmount

  push cMaxVotes; toNamed #maxAmount
  if #maxAmount <. #newAmount then
    failCustom_ #mAX_VOTES_REACHED
  else nop

-- | Vote
vote
  :: forall store pm. (StorageC store pm, NiceParameter pm)
  => Config pm -> Entrypoint [VoteParam] store
vote config = do
  iter $ do
    dupTop2
    stackType @[VoteParam, store, VoteParam, store]

    toField #vProposalKey

    checkIfProposalExist
    stackType @[Proposal pm, VoteParam, store]

    checkVoteLimitReached config
    stackType @[Proposal pm, VoteParam, store]

    duupX @3
    swap
    stackType @[Proposal pm, store, VoteParam, store]

    ensureVotingPeriodIsNotOver
    stackType @[VoteParam, store]

    checkVoterUnfrozenToken
    stackType @[VoteParam, store]

    submitVote

  nil; pair

------------------------------------------------------------------------
-- ProposalMetadata
------------------------------------------------------------------------

proposalMetadata
  :: forall store pm. (StorageC store pm, NiceParameter pm)
  => Entrypoint (View ProposalKey (Proposal pm)) store
proposalMetadata =
  view_ $ do
    checkIfProposalExist

------------------------------------------------------------------------
-- Admin Entrypoints (set votingPeriod, quorum, flush)
------------------------------------------------------------------------

-- | Update voting period of all ongoing and new proposals.
setVotingPeriod
  :: forall store pm. (StorageC store pm)
  => Config pm -> Entrypoint VotingPeriod store
setVotingPeriod Config{..}= do
  dip authorizeAdmin

  toNamed #newValue
  dup
  push cMaxVotingPeriod; toNamed #maxValue
  if #maxValue <. #newValue then
    failCustom_ #oUT_OF_BOUND_VOTING_PERIOD
  else do
    dup
    push cMinVotingPeriod; toNamed #minValue
    if #minValue >. #newValue then
      failCustom_ #oUT_OF_BOUND_VOTING_PERIOD
    else do
      fromNamed #newValue
      stSetField #sVotingPeriod
      nil; pair

-- | Update quroum_threshold. The new quorum_threshold affects
-- all ongoing and new proposals.
setQuorumThreshold
  :: forall store pm. (StorageC store pm)
  => Config pm -> Entrypoint QuorumThreshold store
setQuorumThreshold Config{..} = do
  dip authorizeAdmin

  toNamed #newValue
  dup
  push cMaxQuorumThreshold; toNamed #maxValue
  if #maxValue <. #newValue then
    failCustom_ #oUT_OF_BOUND_QUORUM_THRESHOLD
  else do
    dup
    push cMinQuorumThreshold; toNamed #minValue
    if #minValue >. #newValue then
      failCustom_ #oUT_OF_BOUND_QUORUM_THRESHOLD
    else do
      fromNamed #newValue
      stSetField #sQuorumThreshold
      nil; pair

checkIfProposalIsOver
  :: forall store pm. (NiceParameter pm, StorageC store pm)
  => Config pm -> '[ProposalKey, store] :-> '[((Bool, ProposalKey), Proposal pm), store]
checkIfProposalIsOver config = do
  dupTop2

  checkIfProposalExist
  stackType @[(Proposal pm), ProposalKey, store]

  dup; dip swap; dip $ dip swap;
  stackType @[(Proposal pm), ProposalKey, store, (Proposal pm)]
  toField #pStartDate
  duupX @3

  stToField #sVotingPeriod; int
  add -- add to the start_date
  toNamed #expectedEndDate
  now; toNamed #currentDate

  cond

  dip $ dip swap

  pair; pair

  where
    cond = case (cForceVotingPeriod config) of
      True -> do drop; drop; push True
      False -> do
          if #currentDate >=. #expectedEndDate then
            push True
          else
            push False


-- Used in "flush". When unfreezing the tokens of voters/proposers
-- It is possible that the currentBalance is less than the frozenValue due to
-- the fact that admin can burn/transfer frozen token.
-- This function ensures that the "unfreeze" function will use the correct amount
-- to unfreeze.
checkBalanceLessThanFrozenValue :: forall store pm. (StorageC store pm)
  => [Natural, Address, store, Proposal pm, ProposalKey, [Operation]]
  :-> [Natural, Address, store, Proposal pm, ProposalKey, [Operation]]
checkBalanceLessThanFrozenValue = do
  toNamed #unfreeze_value
  swap; dip swap
  stackType @[Address, store, "unfreeze_value" :! Natural, Proposal pm, ProposalKey, [Operation]]
  dupTop2
  push frozenTokenId; swap; pair

  stGet #sLedger; ifSome nop $ do
    failCustom_ #pROPOSER_NOT_EXIST_IN_LEDGER

  stackType @[Natural, Address, store, "unfreeze_value" :! Natural, Proposal pm, ProposalKey, [Operation]]
  toNamed #actual_frozen_value
  dig @3
  stackType @["unfreeze_value" :! Natural, "actual_frozen_value" :! Natural, Address, store, Proposal pm, ProposalKey, [Operation]]

  dupTop2
  if #unfreeze_value >. #actual_frozen_value then do
    drop; fromNamed #actual_frozen_value
  else do
    dip drop; fromNamed #unfreeze_value

burnFrozenToken
  :: forall store pm s. StorageC store pm
  => (Address : Natural : store : s)
  :-> store : s
burnFrozenToken = do
  dip $ do push frozenTokenId; pair
  dig @2
  debitFrom

-- | Burn the 'slash_amount' calculated by 'cRejectedProposalReturnValue'
-- If the 'slash_amount' is equal or bigger than the 'frozen' amount for that
-- particular proposal, we only burn value equal to the 'frozen' amount.
-- This is to avoid 'slash_amount' being abitrary large value, and we could burn
-- the whole proposer frozen balance.
burnSlashAmount
  :: forall store pm s. StorageC store pm
  => ("slash_amount" :! Natural) : Natural : Address : store : s
  :-> ("slash_amount" :! Natural) : Natural : Address : store : s
burnSlashAmount = do
  dupTop2
  swap; dip (fromNamed #slash_amount)
  sub; isNat
  ifSome
    (do
      drop
      stackType @("slash_amount" :! Natural : Natural : Address : store : s)
      dig @3; duupX @2; fromNamed #slash_amount -- Use the slash value
      duupX @5
      stackType @(Address : Natural : store : "slash_amount" :! Natural : Natural : Address : s)
    )
    (do
      stackType @("slash_amount" :! Natural : Natural : Address : store : s)
      dig @3; duupX @3 -- Use the frozen balance value
      duupX @5
      stackType @(Address : Natural : store : "slash_amount" :! Natural : Natural : Address : s)
    )
  burnFrozenToken
  stackType @(store : "slash_amount" :! Natural : Natural : Address : s)
  swap; dip $ do swap; dip swap

unfreezeProposerToken
  :: forall store pm. (NiceParameter pm, StorageC store pm)
  => Config pm -> Bool -> [Proposal pm, store, ProposalKey, [Operation]]
  :-> [Proposal pm, store, ProposalKey, [Operation]]
unfreezeProposerToken Config{..} isAccepted = do

  handleSlashed
  stackType @[Natural, Address, store, Proposal pm, ProposalKey, [Operation]]

  checkBalanceLessThanFrozenValue
  unfreeze

  stackType @[store, Proposal pm, ProposalKey, [Operation]]
  swap
  where
    handleSlashed =
      case isAccepted of
        True -> do
          stackType @[Proposal pm, store, ProposalKey, [Operation]]
          getField #pProposerFrozenToken
          dip $ getField #pProposer
          dip $ dip swap
          stackType @[Natural, Address, store, Proposal pm, ProposalKey, [Operation]]

        False -> do
          dup
          cRejectedProposalReturnValue
          stackType @["slash_amount" :! Natural, Proposal pm, store, ProposalKey, [Operation]]
          dip $ do
            getField #pProposerFrozenToken
            dip $ getField #pProposer
            dip $ dip swap
          stackType @["slash_amount" :! Natural, Natural, Address, store, Proposal pm, ProposalKey, [Operation]]
          burnSlashAmount

          -- Calculate unfreeze amount
          swap; dip (fromNamed #slash_amount)
          sub; isNat
          ifSome nop $ do
            -- If the result is negative, we unfreeze 0 tokens.
            -- The frozen token balance associated with that proposal will be burn
            -- via 'burnSlashAmount'
            push (0 :: Natural)
            stackType @[Natural, Address, store, Proposal pm, ProposalKey, [Operation]]

unfreezeVoterToken
  :: forall store pm. (NiceParameter pm, StorageC store pm)
  => [Proposal pm, store, ProposalKey, [Operation]] :-> [Proposal pm, store, ProposalKey, [Operation]]
unfreezeVoterToken = do
  dup; dig @2; swap
  toField #pVoters
  iter $ do
    unpair
    swap
    unfreeze
  swap

handleProposalIsOver
  :: forall store pm. (NiceParameter pm, StorageC store pm)
  => Config pm -> '[((Bool, ProposalKey), Proposal pm), store, [Operation]] :-> '[store, [Operation]]
handleProposalIsOver config@Config{..} = do
  unpair; unpair

  if Holds then do
    -- | start unfreezing process
    swap; dip swap
    stackType @[Proposal pm, store, ProposalKey, [Operation]]

    getField #pUpvotes
    dip $ getField #pDownvotes
    add; toNamed #proposalTotalVote

    duupX @3
    stToField #sQuorumThreshold; toNamed #sQuorumThreshold

    if #sQuorumThreshold <=. #proposalTotalVote then do
      getFieldNamed #pUpvotes
      dip $ getFieldNamed #pDownvotes
      if #pUpvotes >. #pDownvotes then do
        unfreezeProposerToken config True
        unfreezeVoterToken
        cDecisionLambda
        unpair
        stackType @[[Operation], store, ProposalKey, [Operation]]
        dig @3
        iter cons
        swap; dip swap
      else do
        -- | Reject proposal if (upvote <= downvote)
        unfreezeProposerToken config False
        unfreezeVoterToken
        drop

    else do
      -- | Reject proposal regardless of upvotes
      unfreezeProposerToken config False
      unfreezeVoterToken
      drop

    stackType @[store, ProposalKey, [Operation]]

    swap
    stDelete #sProposals

  else do
    -- | Add the proposalId back to #sProposalKeyListSortByDate
    -- since we initially set this to []
    dip drop

    dip $ stGetField #sProposalKeyListSortByDate
    cons
    stSetField #sProposalKeyListSortByDate

-- | Flush all proposals that passed their voting period
-- If the proposal's total vote >= quorum_theshold and it's upvote > downvote
-- the proposal is accepted.
-- Otherwise it will be rejected.
flush
  :: forall store pm. (NiceParameter pm, StorageC store pm)
  => Config pm -> Entrypoint () store
flush config = do
  dip authorizeAdmin
  drop
  dup
  stToField #sProposalKeyListSortByDate
  stackType @[[ProposalKey], store]

  map (checkIfProposalIsOver config)

  stackType @[[((Bool, ProposalKey), Proposal pm)], store]

  swap; push []; stSetField #sProposalKeyListSortByDate -- set to empty list
  swap

  stackType @[[((Bool, ProposalKey), Proposal pm)], store]
  nil; swap; dip swap
  stackType @[[((Bool, ProposalKey), Proposal pm)], store, [Operation]]

  iter (handleProposalIsOver config)
  swap; pair
