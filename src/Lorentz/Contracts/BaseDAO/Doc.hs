-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.BaseDAO.Doc
   ( introductoryDoc
   , priorChecksDoc

   , callFA2Doc
   , transferDoc
   , balanceOfDoc
   , tokenMetadataRegistryDoc
   , updateOperatorsDoc

   , transferOwnershipDoc
   , acceptOwnershipDoc
   , migrateDoc
   , confirmMigrationDoc

   , proposeDoc
   , voteDoc
   , setVotingPeriodDoc
   , setQuorumThresholdDoc
   , flushDoc
   , dropProposalDoc

   , burnDoc
   , mintDoc
   , transferContractTokensDoc

   , getVotePermitCounterDoc

   , callCustomDoc
   ) where

import Lorentz

import Util.Markdown

introductoryDoc :: Markdown
introductoryDoc = [md|
  It contains standard FA2 entrypoints, plus some extra ones including proposal and
  migration entrypoints. It supports two types of token_id - frozen (token_id = 1) and unfrozen (token_id = 0).
  |]

priorChecksDoc :: Markdown
priorChecksDoc = [md|
  These properties belong to all entrypoints of the contract.
  |]

callFA2Doc :: Markdown
callFA2Doc = [md|
  Entrypoint to be called if you want to use one of FA2 entrypoints.
  |]

transferDoc :: Markdown
transferDoc = [md|
  Transfer tokens between a given account and each account from the given list.

  It serves multiple purposes:
  * If transaction `"from"` address equals to the admin address presented in storage, then  it is allowed for
  any address and both `frozen` and `unfrozen` tokens
  * Otherwise, it is allowed to transfer money only if `from` address equals to sender address or have sender as an operator
  It is also prohibited to send frozen tokens in this case.
  |]

balanceOfDoc :: Markdown
balanceOfDoc = [md|
  Returns the balance of specified address in ledger.
  The entrypoint supports both frozen and unfrozen tokens.
  |]

tokenMetadataRegistryDoc :: Markdown
tokenMetadataRegistryDoc = [md|
  Returns contract address that holds token metadata.
  Token metadata will contain the DAO metadata.
  |]

updateOperatorsDoc :: Markdown
updateOperatorsDoc = [md|
  Updates operators. There are 2 different opportunities:

  * Add operator - updates operators with a new pair of `(operator, owner)`.
  If this pair already exists, this entrypoint does nothing
  * Remove operator - updates operators, removing the existing pair of `(operator, owner)`.
  If there is no such key, this entrypoint does nothing.

  All tokens passed to this entrypoint as a part of an argument must be unfrozen.
  Each owner must be equal to sender, or the entrypoint fails.
  |]

transferOwnershipDoc :: Markdown
transferOwnershipDoc  = [md|
  Asks an Address to become an admin. Can be called only by current administrator. The admin duties transfer only when the
  requested address accepts ownership. If called multiple times, only the last called address can accept ownership.
  |]

acceptOwnershipDoc :: Markdown
acceptOwnershipDoc = [md|
  Accepts the administrator privelege.
  Only works when the sender was asked to become an admin and only if it was asked by the current admin.
  |]

migrateDoc :: Markdown
migrateDoc = [md|
  Asks an address to migrate the contract to it.
  The contract is not considered migrated, until it receives confirm_migration call.
  |]

confirmMigrationDoc :: Markdown
confirmMigrationDoc = [md|
  Confirms migration of a contract to the sender address.
  After a successful call the contract will be set to migrated state where no operations are possible.
  |]

proposeDoc :: Markdown
proposeDoc = [md|
  Saves the proposal with specific id and freezes the amount of sender tokens equal to the given amount.
  The sender must have enough unfrozen tokens.
  The sender's amount of frozen tokens is increased by proposal ammount. And the amount of unfrozen one
  is decreased by the same value.
  |]

voteDoc :: Markdown
voteDoc = [md|
  For each vote params in a given list vote for the proposal with that id. Thus the sender can vote many proposals
  (or one proposal multiple times) in a single call.
  The sender must have an amount required for all votings.
  |]

setVotingPeriodDoc :: Markdown
setVotingPeriodDoc = [md|
  Updates how long the voting period would last.
  It affects all ongoing proposals and all created afterwards.
  |]

setQuorumThresholdDoc :: Markdown
setQuorumThresholdDoc = [md|
  Updates the quorum threshold with a given value.
  It affects all ongoing proposals and all created afterwards.
  |]


flushDoc :: Markdown
flushDoc = [md|
  Finish voting process on all proposals where the voting period is over.
  Returns an amount to the proposer, determined by the result of voting.
  There is a possibility of some tokens being lost due to administrator perform
  of burn or transfer operation.
  If the proposal is accepted, the decision lambda is called.
  |]

dropProposalDoc :: Markdown
dropProposalDoc = [md|
  Delete a finished and accepted proposal that is not flushed. Tokens frozen for this
  proposal are returned to the proposer and voters in full. The decision lambda is skipped.

  This entrypoint should only be used when there is a proposal that is stuck due to having a
  failing decision lambda.
  |]

burnDoc :: Markdown
burnDoc = [md|
  Reduces the amount of tokens of the given address. Can be performed only
  only if the given address has enough tokens to burn.
  |]

mintDoc :: Markdown
mintDoc = [md|
  Provides the amount of tokens of the given address.
  |]

transferContractTokensDoc :: Markdown
transferContractTokensDoc = [md|
  This entrypoint can be used by the administrator
  to transfer tokens owned (or operated) by this contract in another FA2 contract.
  Unlike the others, this entrypoint can be used after contract is migrated.
  |]

getVotePermitCounterDoc :: Markdown
getVotePermitCounterDoc = [md|
  Returns the next nonce value with which a permit should be created.

  Return value increases by number of votes where a permit was provided
  with each successful call of an entrypoint.
  |]

callCustomDoc :: Markdown
callCustomDoc = [md|
  Additional entrypoints specific to the given specific DAO.
  |]
