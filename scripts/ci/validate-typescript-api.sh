#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2021 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

mkdir -p /tmp/ts_generated
rm -rf /tmp/ts_generated/*
baseDAO-ligo-meta generate-typescript --target=/tmp/ts_generated
diff_res=$(diff /tmp/ts_generated typescript/baseDAO/src/generated)
diff_exit_code="$?"
if [ $diff_exit_code -ne 0 ]; then
    echo "Contract interface has changed, and typescript Api require re-generation."
    echo "$diff_res"
    echo "run 'make typescript' locally"
    exit 1
fi
