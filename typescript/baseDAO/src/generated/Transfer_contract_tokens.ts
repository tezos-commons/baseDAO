import {Lambda} from '../common';
export interface Transfer_contract_tokens {
  contract_address: string;
  params: Transfer_contract_tokensParams;
};
export type Transfer_contract_tokensParams = Array<Transfer_contract_tokensParamsItem>;
export interface Transfer_contract_tokensParamsItem {
  from_: string;
  txs: Transfer_contract_tokensParamsItemTxs;
};
export type Transfer_contract_tokensParamsItemTxs = Array<Transfer_contract_tokensParamsItemTxsItem>;
export interface Transfer_contract_tokensParamsItemTxsItem {
  to_: string;
  token_id: number;
  amount: number;
};
