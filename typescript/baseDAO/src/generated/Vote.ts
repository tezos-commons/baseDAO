import {Lambda} from '../common';
export type Vote = Array<VoteItem>;
export interface VoteItem {
  from: string;
  proposal_key: string;
  vote_amount: number;
  vote_type: boolean;
  permit?: VoteItemPermit;
};
export interface VoteItemPermit {
  key: string;
  signature: string;
};
