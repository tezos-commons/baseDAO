import {Lambda} from '../common';
export type Update_operators = Array<Update_operatorsItem>;
export type Update_operatorsItem =
  | Update_operatorsItemAdd_operator
  | Update_operatorsItemRemove_operator
export interface Update_operatorsItemAdd_operatorItem {
  owner: string;
  operator: string;
  token_id: number;
};
export interface Update_operatorsItemAdd_operator {
  add_operator: Update_operatorsItemAdd_operatorItem;
};
export interface Update_operatorsItemRemove_operatorItem {
  owner: string;
  operator: string;
  token_id: number;
};
export interface Update_operatorsItemRemove_operator {
  remove_operator: Update_operatorsItemRemove_operatorItem;
};
