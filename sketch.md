# Entrypoints

* FA2 entrypoints
* Voting entrypoints:
  + `propose(proposal)`
  + `vote(proposalId, bool)`
  + `flush()`
* Admin entrypoints:
  + Change the admin (perhaps in two steps)
  + `transferViaAdmin` to transfer FA2 tokens of this contract.
  + Migrate the DAO: to be specified.

`proposal` can be `map string string` or `pair bytes (map string string)` where `bytes` serves as `proposalId`.
`proposalId` can be `bytes` or `nat` or `string` or `int`.

# Storage

* Values set during origination (immutable or updateable only by admin).
Maybe some of them can be specified in compile time and thus hardcoded in Michelson code for efficiency.
  + Bond requirements: can be `lambda nat unit` that fails on invalid bond value. `nat` represents token amount.
  + Period length: `nat` that stores seconds.
  + Quorum threshold: `nat` representing token amount.
  + Proposal requirements: can be `lambda proposal unit` that fails on invalid proposals (e. g. not having certain keys).
    If it fails, proposal can't be proposed.
  + Admin address.
  + Decision lambda: has the same type (`lambda proposal unit`), but is called only when `proposal` is approved by voting procedure.
  + The address to send tokens to in order to freeze them
  + ID of the frozen token.
  + "Freeze" address.
* FA2 storage (balances, operators, etc.).
* Information about active proposals: to be specified.

# Questions

1. FA2 can have multiple `token_id`s.
How many `token_id`s should be in this particular contract?
Apparently there is one special id (frozen token).
Are there tokens with other IDs, is there number limited?
2. In the document there is this on-chain information: "Informative entrypoint to tell you which FA2 it is".
What does it mean?
3. Who can submit proposals?
Are the requirements for submitting proposals different from voting?
Does the proposer have to transfer a certain amount of tokens of "frozen token_id" to a special address in order to submit a proposal?
4. When a proposal is rejected, "bonds locked by proposal creators are either returned or slashed and returned".
How do we decide whether to return the whole bond or a slashed amount?
