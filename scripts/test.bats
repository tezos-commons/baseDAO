#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

exe="baseDAO"

@test "Invoking with --help" {
  "$exe" --help
}

@test "Invoking with --version" {
  "$exe" --version
}

@test "List available contracts" {
  "$exe" list
}

@test "Print BaseDAO" {
  "$exe" print -n TrivialDAO
}

@test "Print initial BaseDAO storage" {
  "$exe" storage-TrivialDAO --admin KT1VdyhR9JsmfpxdGf2hMBo1f4Z2TCogp8Th
}

@test "Document BaseDAO" {
  "$exe" document -n TrivialDAO
}
