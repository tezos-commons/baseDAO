import {Lambda} from '../common';
export type Transfer = Array<TransferItem>;
export interface TransferItem {
  from_: string;
  txs: TransferItemTxs;
};
export type TransferItemTxs = Array<TransferItemTxsItem>;
export interface TransferItemTxsItem {
  to_: string;
  token_id: number;
  amount: number;
};
