import {Lambda} from '../common';
export interface Propose {
  frozen_token: number;
  proposal_metadata: ProposeProposal_metadata;
};
export type ProposeProposal_metadata = Map<string,string>;
