import {Accept_ownership} from  './Accept_ownership';
import {ConcreteEp} from  './ConcreteEp';
import {CustomEp} from  './CustomEp';
import {Drop_proposal} from  './Drop_proposal';
import {Flush} from  './Flush';
import {Freeze} from  './Freeze';
import {Propose} from  './Propose';
import {Transfer_contract_tokens} from  './Transfer_contract_tokens';
import {Transfer_ownership} from  './Transfer_ownership';
import {Unfreeze} from  './Unfreeze';
import {Unstake_vote} from  './Unstake_vote';
import {Update_delegate} from  './Update_delegate';
import {Vote} from  './Vote';
export type Parameter =
  | Accept_ownership
  | ConcreteEp
  | CustomEp
  | Drop_proposal
  | Flush
  | Freeze
  | Propose
  | Transfer_contract_tokens
  | Transfer_ownership
  | Unfreeze
  | Unstake_vote
  | Update_delegate
  | Vote
