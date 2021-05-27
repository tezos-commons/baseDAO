// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"

[@inline]
let quorum_denominator_int = int(quorum_denominator)
  // Hopefuly this will be optimized by the compiler and does not actually
  // call `int` for every access to this value

// Multiply two quorum_fractions
//
// We store fractions by storing only the numerator and denominator is always
// assumed to be quorum_denominator. So here qt_1, actually represents the
// value qt_1.numerator/ quorum_denominator and qt_2 represents the value
// qt_2.numerator/ quorum_denominator. So the product of the two is
// qt_1.numerator * qt_2.numerator / (quorum_denominator *
// quorum_denominator).  But since we store x as x * quorum_denominator,
// the result here would be qt_1.numerator * qt_2.numerator /
// quorum_denominator. This will also retain the required precision of
// 1/quorum_denominator.
[@inline]
let fmul(qt_1, qt_2 : quorum_fraction * quorum_fraction): quorum_fraction =
  { numerator = (qt_1.numerator * qt_2.numerator) / quorum_denominator_int }

// Divide the first fraction by the second Here qt_1, actually represents the
// value qt_1.numerator/ quorum_denominator and qt_2 represents the value
// qt_2.numerator/ quorum_denominator. So the division can be expressed as
// (qt_1.numerator * quorum_denominator) / (quorum_denominator *
// qt_2.numerator) But since we store x as x * quorum_denominator, the result
// here would be (qt_1.numerator * quorum_denominator_int) / qt_2.numerator
[@inline]
let fdiv(qt_1, qt_2 : quorum_fraction * quorum_fraction): quorum_fraction =
  { numerator = (qt_1.numerator * quorum_denominator_int) / qt_2.numerator }

[@inline]
let fadd(qt_1, qt_2 : quorum_fraction * quorum_fraction): quorum_fraction =
  {numerator = qt_1.numerator + qt_2.numerator }

[@inline]
let fsub(qt_1, qt_2 : quorum_fraction * quorum_fraction): quorum_fraction =
  { numerator = qt_1.numerator - qt_2.numerator }

[@inline]
let bound_qt (qt, min_qt, max_qt : quorum_fraction * quorum_fraction * quorum_fraction)
    : quorum_fraction =
  if (qt.numerator > max_qt.numerator) then max_qt
  else if (qt.numerator < min_qt.numerator) then min_qt
  else qt

[@inline]
// Returns the 'cycle' number of which the given 'stage' number is part of.
let stage_to_cycle (p: nat): nat = (p + 1n) / 2n

[@inline]
let to_signed(n : unsigned_quorum_fraction): quorum_fraction
  = { numerator = int(n.numerator) }

[@inline]
let fraction_to_quorum_fraction(n, d : nat * nat): quorum_fraction
  = { numerator = int((n * quorum_denominator) / d) }

[@inline]
let to_unsigned(n : quorum_fraction): unsigned_quorum_fraction =
  { numerator =
      match is_nat(n.numerator) with
      | Some n -> n
      | None   -> (failwith("BAD_STATE") : nat)
  }

let update_quorum(current_period, store, config : nat * storage * config): storage =
  let current_cycle = stage_to_cycle(current_period) in
  if store.quorum_threshold_at_cycle.last_updated_cycle = current_cycle
  then store // Quorum has been updated in this period, so no change is required.
  else if current_cycle > store.quorum_threshold_at_cycle.last_updated_cycle
  then
    let previous_staked = store.quorum_threshold_at_cycle.staked in
    let previous_participation = fraction_to_quorum_fraction(previous_staked, config.governance_total_supply) in
    let old_quorum = to_signed(store.quorum_threshold_at_cycle.quorum_threshold) in
    let quorum_change = config.quorum_change in
    let possible_new_quorum =
      // old_quorum + (previous_participation - old_quorum) * quorum_change
      fadd(old_quorum, fmul(quorum_change, fsub(previous_participation, old_quorum))) in
    let one_plus_max_change_percent = { numerator = config.max_quorum_change.numerator + quorum_denominator } in
    let min_new_quorum = fdiv(old_quorum, one_plus_max_change_percent) in
    let max_new_quorum = fmul(old_quorum, one_plus_max_change_percent) in

    let config_min_qt = config.min_quorum_threshold in
    let config_max_qt = config.max_quorum_threshold in
    let new_quorum = bound_qt(possible_new_quorum, min_new_quorum, max_new_quorum) in
    let new_quorum = bound_qt(new_quorum, config_min_qt, config_max_qt) in
    { store with quorum_threshold_at_cycle =
      { quorum_threshold = to_unsigned(new_quorum)
      ; last_updated_cycle = current_cycle
      ; staked = 0n;
      }
    }
  else store
