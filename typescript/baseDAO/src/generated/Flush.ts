import {Lambda} from '../common';
export type Flush =
  | number
  | FlushFlush_skip
  | FlushFlush_target
export type FlushFlush_skip = Array<string>;
export type FlushFlush_target = Array<string>;
