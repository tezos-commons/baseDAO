#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2021 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# This script takes a list of filepaths that are not going to be checked
# for updates.

set -e

# There is no trivial way to put "find" output to array:(
files=()
while IFS=  read -r -d $'\0'; do
    files+=("$REPLY")
done < <(find . -name "*.cabal" -print0)

files+=("cabal.project" "cabal.project.freeze")
files_to_verify=()
excluded=false

for file in "${files[@]}"; do
    for excluded_file in "$@"; do
        if [[ $file == "$excluded_file" || $file == *".stack-work"* ]]; then
            excluded=true
        fi
    done
    if ! $excluded; then
        cp "$file" "${file}_backup"
        files_to_verify+=("$file")
    fi
    excluded=false
done

stack2cabal

for file in "${files_to_verify[@]}"; do
    set +e
    diff_res=$(diff "$file" "${file}_backup")
    diff_exit_code="$?"
    set -e
    if [ $diff_exit_code -ne 0 ]; then
        echo "file \"$file\" has changed"
        echo "$diff_res"
        echo "run ./scripts/generate-cabal-files.sh locally"
        echo "and update cabal related fiels"
        exit 1
    fi
done

find . -name "*_backup" -type f -delete
echo "cabal files are up to date"
