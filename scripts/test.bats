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

@test "Print Basic" {
  "$exe" print -n Basic
}

@test "Print initial Basic storage" {
  "$exe" storage-Basic
}

@test "Document Basic" {
  "$exe" document -n Basic
}
