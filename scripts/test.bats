#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

ligo_exe="baseDAO-ligo-meta"

@test "Print LIGO metadata" {
  "$ligo_exe" print-metadata --frozen-token-symbol=POG > metadata.json
}
