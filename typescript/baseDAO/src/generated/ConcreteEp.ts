import {Lambda} from '../common';
export type ConcreteEp =
  | {}
  | {}
  | ConcreteEpPropose
  | ConcreteEpTransfer_contract_tokens
  | string
export interface ConcreteEpProposeItem {
  from: string;
  frozen_token: number;
  proposal_metadata: string;
};
export interface ConcreteEpPropose {
  propose: ConcreteEpProposeItem;
};
export interface ConcreteEpTransfer_contract_tokensItem {
  contract_address: string;
  params: ConcreteEpTransfer_contract_tokensParams;
};
export interface ConcreteEpTransfer_contract_tokens {
  transfer_contract_tokens: ConcreteEpTransfer_contract_tokensItem;
};
export type ConcreteEpTransfer_contract_tokensParams =
  Array<ConcreteEpTransfer_contract_tokensParamsItem>;
export interface ConcreteEpTransfer_contract_tokensParamsItem {
  from_: string;
  txs: ConcreteEpTransfer_contract_tokensParamsItemTxs;
};
export type ConcreteEpTransfer_contract_tokensParamsItemTxs =
  Array<ConcreteEpTransfer_contract_tokensParamsItemTxsItem>;
export interface ConcreteEpTransfer_contract_tokensParamsItemTxsItem {
  to_: string;
  token_id: number;
  amount: number;
};
