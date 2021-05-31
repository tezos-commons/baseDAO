import {Accept_ownership} from  './Accept_ownership';
import {CallCustom} from  './CallCustom';
import {Drop_proposal} from  './Drop_proposal';
import {Flush} from  './Flush';
import {Freeze} from  './Freeze';
import {Propose} from  './Propose';
import {Transfer_contract_tokens} from  './Transfer_contract_tokens';
import {Transfer_ownership} from  './Transfer_ownership';
import {Unfreeze} from  './Unfreeze';
import {Update_delegate} from  './Update_delegate';
import {Vote} from  './Vote';
export type Parameter =
  | Accept_ownership
  | CallCustom
  | Drop_proposal
  | Flush
  | Freeze
  | Propose
  | Transfer_contract_tokens
  | Transfer_ownership
  | Unfreeze
  | Update_delegate
  | Vote
