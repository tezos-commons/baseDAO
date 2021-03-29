#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

exe="baseDAO"
ligo_exe="baseDAO-ligo-meta"
# We expect all these contracts to be present.
contracts="GameDAO MetadataCarrier RegistryDAO TreasuryDAO TrivialDAO"

@test "Invoking with --help" {
  "$exe" --help
}

@test "Invoking with --version" {
  "$exe" --version
}

@test "List available contracts" {
  list=`"$exe" list`
  for name in $contracts;
  do
    echo "$list" | grep "$name"
  done
}

@test "Print contracts" {
  for name in $contracts;
  do
    "$exe" print -n "$name"
  done
}

@test "Print initial TrivialDAO storage" {
  "$exe" storage-TrivialDAO --admin KT1VdyhR9JsmfpxdGf2hMBo1f4Z2TCogp8Th \
                            --metadata-host-address KT1VdyhR9JsmfpxdGf2hMBo1f4Z2TCogp8Th \
                            --metadata-key ""
}

@test "Document contracts" {
  for name in $contracts;
  do
    [[ "$name" == MetadataCarrier ]] || "$exe" document -n "$name"
  done
}

@test "Print metadata" {
  "$exe" print-metadata --frozen-token-symbol=POG > metadata.json
}

@test "Print LIGO metadata" {
  "$ligo_exe" print-metadata --frozen-token-symbol=POG > metadata.json
}

@test "Typecheck Lorentz contracts" {
  for name in $contracts;
  do
    "$exe" print -n "$name" -o out.tz
    tezos-client --mode mockup typecheck script out.tz
  done
}
