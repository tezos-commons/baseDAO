import {Lambda} from '../common';
export interface Balance_of {
  requests: Balance_ofRequests;
  callback: string;
};
export type Balance_ofRequests = Array<Balance_ofRequestsItem>;
export interface Balance_ofRequestsItem {
  owner: string;
  token_id: number;
};
