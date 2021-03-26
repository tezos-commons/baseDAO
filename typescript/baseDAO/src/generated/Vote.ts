import {Lambda} from '../common';
export type Vote = Array<VoteItem>;
export interface VoteItem {
  argument: VoteItemArgument;
  permit?: VoteItemPermit;
};
export interface VoteItemArgument {
  proposal_key: string;
  vote_type: boolean;
  vote_amount: number;
};
export interface VoteItemPermit {
  key: string;
  signature: string;
};
