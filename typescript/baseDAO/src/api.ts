// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

import { ContractMethod , Signer, TransactionWalletOperation,
         Contract, ContractAbstraction,
         ContractProvider, TezosToolkit, MichelsonMap
       } from '@taquito/taquito';
import { InMemorySigner, importKey } from '@taquito/signer';

import { Parameter } from './generated/Parameter';
import { CallCustom } from './generated/CallCustom';
import { Accept_ownership } from './generated/Accept_ownership';
import { Burn } from './generated/Burn';
import { Balance_of } from './generated/Balance_of';
import { Drop_proposal } from './generated/Drop_proposal';
import { Flush } from './generated/Flush';
import { Get_total_supply } from './generated/Get_total_supply';
import { Mint } from './generated/Mint';
import { Propose } from './generated/Propose';
import { Set_quorum_threshold } from './generated/Set_quorum_threshold';
import { Set_voting_period } from './generated/Set_voting_period';
import { Transfer } from './generated/Transfer';
import { Transfer_contract_tokens } from './generated/Transfer_contract_tokens';
import { Transfer_ownership } from './generated/Transfer_ownership';
import { Update_operators } from './generated/Update_operators';
import { Vote } from './generated/Vote';
import { Freeze } from './generated/Freeze';
import { Unfreeze } from './generated/Unfreeze';
import { Get_vote_permit_counter } from './generated/Get_vote_permit_counter';

function println(s: string) {
  console.log(s);
}

type Callback = (Contract) => ContractMethod<ContractProvider>

type ExtractMapKey<MapType> = MapType extends Map<infer R, infer V> ? R : null;
type ExtractMapValue<MapType> = MapType extends Map<infer R, infer V> ? V : null;

const unit = ["Unit"]; //https://github.com/ecadlabs/taquito/issues/526

/**
 * Check equality of objects via JSON.stringify().
 */
function isSame(exp: any, got: any): void {
  let exp_str = JSON.stringify(exp);
  let got_str = JSON.stringify(got);
  if (exp_str !== got_str) {
    throw {expected: exp_str, got: got_str};
  }
}

/**
 * Wraps BaseDAO Contract methods
 */
export class BaseDAOContract {
  private contractAddr: string;
  private nodeAddr: string;
  private senderSk: string;
  private contract: undefined | Promise<ContractAbstraction<ContractProvider>>;

  public debug: boolean;

  public lastOperationHash: undefined | string;

  constructor(nodeAddr: string, senderSk: string, contractAddr: string) {
    this.contractAddr = contractAddr;
    this.nodeAddr = nodeAddr;
    this.senderSk = senderSk;
    this.debug = false;
  }

  public parameterSchema(): Promise<object> {
    return this.inspectContract(contract => contract.parameterSchema.ExtractSchema());
  }

  public storageSchema(): Promise<object> {
    return this.inspectContract(contract => contract.schema.ExtractSchema());
  }

  /**
   * Ensure the storage of the contract matches with provided the reference object
   * @param storageSchemaTargetJson - The reference schema as a plain object.
   * @returns A promise that yields nothing on success or throw on fail.
   */
  public ensureStorageSchema(storageSchemaTargetJson: Object): Promise<void> {
      let checker = function(contract: ContractAbstraction<ContractProvider>): void {
        isSame(storageSchemaTargetJson, contract.schema.ExtractSchema());
      }
      return this.inspectContract(checker);
  }

  /**
   * Ensure the parameter of the contract matches with provided the reference object
   * @param storageSchemaTargetJson - The reference schema as a plain object.
   * @returns A promise that yields nothing on success or throw on fail.
   */
  public ensureParamSchema(paramSchemaTargetJson: Object): Promise<void> {
      let checker = function(contract: ContractAbstraction<ContractProvider>): void {
        isSame(paramSchemaTargetJson, contract.parameterSchema.ExtractSchema());
      };
      return this.inspectContract(checker);
  }

  private initContract(): Promise<ContractAbstraction<ContractProvider>> {
    if (this.contract === undefined) {
      const Tezos = new TezosToolkit(this.nodeAddr);
      Tezos.setProvider({ signer: new InMemorySigner(this.senderSk) });
      this.contract = Tezos.contract.at(this.contractAddr);
    }
    return this.contract;
  }

  public setSender(senderSk: string) {
    this.senderSk = senderSk;
    return this;
  }

  public inspectContract(callback: (c: ContractAbstraction<ContractProvider>) => any): Promise<any> {
    return this.initContract().then(contract => {
      return callback(contract);
      })
  }

  private withContract(callback: Callback): Promise<string | void> {

    // Initialize contract if it hasn't been initialized
    return this.initContract().then(callback)
        .then((method:ContractMethod<ContractProvider>) => {
          if (this.debug) {
            let tp = method.toTransferParams();
            println(JSON.stringify(tp, null, 2));
          }
          return method.send();
        })
        .then((op: any): string => {
          println(`Waiting for ${op.hash} to be confirmed...`);
          return op.confirmation(1).then(() => {
            this.lastOperationHash = op.hash;
            return this;
          });
        });
  }

  // entrypoint methods
  balance_of(arg: Balance_of): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.balance_of(arg.requests, arg.callback));
  }

  burn(arg: Burn): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.burn(arg.from_, arg.token_id, arg.amount));
  }

  accept_ownership(): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.accept_ownership(unit));
  }

  call_custom(arg: CallCustom): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.callCustom(arg[0], arg[1]));
  }

  drop_proposal(arg: Drop_proposal): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.drop_proposal(arg));
  }

  flush(arg: Flush): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.flush(arg));
  }

  get_total_supply(arg: Get_total_supply): Promise<string|void> {
    return this.withContract(
      contract =>  contract.methods.get_total_supply(arg.voidParam, arg.voidResProxy));
  }

  get_vote_permit_counter(arg: Get_vote_permit_counter): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.get_vote_permit_counter(arg.voidParam, arg.voidResProxy));
  }

  mint(arg: Mint): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.mint(arg.to_, arg.token_id, arg.amount));
  }

  propose(arg: Propose): Promise<string|void> {
    let proposalMetadata : MichelsonMap<ExtractMapKey<Propose["proposal_metadata"]>, ExtractMapValue<Propose["proposal_metadata"]>> = new MichelsonMap();
    for (let [k, v] of arg.proposal_metadata.entries()) {
      proposalMetadata.set(k, v);
    }

    return this.withContract(
      contract => contract.methods.propose(arg.frozen_token, proposalMetadata));
  }

  set_quorum_threshold(arg: Set_quorum_threshold): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.set_quorum_threshold(arg));
  }

  set_voting_period(arg: Set_voting_period): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.set_voting_period(arg));
  }

  transfer(arg: Transfer): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.transfer(arg));
  }

  transfer_contract_tokens(arg: Transfer_contract_tokens): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.transfer_contract_tokens(arg.contract_address, arg.params));
  }

  transfer_ownership(arg: Transfer_ownership): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.transfer_ownership(arg));
  }

  update_operators(arg: Update_operators) {

    return this.withContract(
      contract => contract.methods.update_operators(arg));
  }

  vote(arg: Vote) {
    return this.withContract(
      contract => contract.methods.vote(arg));
  }

  freeze(arg: Freeze) {
    return this.withContract(
      contract => contract.methods.freeze(arg));
  }

  unfreeze(arg: Freeze) {
    return this.withContract(
      contract => contract.methods.unfreeze(arg));
  }
}
