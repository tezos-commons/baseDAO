import {Lambda} from '../common';
export type Vote = Array<VoteItem>;
export interface VoteItem {
  proposal_key: string;
  vote_type: boolean;
  vote_amount: number;
  permit?: VoteItemPermit;
};
export interface VoteItemPermit {
  key: string;
  signature: string;
};
