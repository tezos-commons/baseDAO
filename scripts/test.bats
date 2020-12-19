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
  "$exe" storage-TrivialDAO --admin KT1VdyhR9JsmfpxdGf2hMBo1f4Z2TCogp8Th \
                            --metadata-host-address KT1VdyhR9JsmfpxdGf2hMBo1f4Z2TCogp8Th \
                            --metadata-key ""
}

@test "Document BaseDAO" {
  "$exe" document -n TrivialDAO
}

@test "Print metadata" {
  "$exe" print-metadata > metadata.json
}
