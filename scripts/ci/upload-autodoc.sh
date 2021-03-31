#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2019-2021 Tocqueville Group
#
# SPDX-License-Identifier: AGPL-3.0-or-later

set -e -o pipefail

git config --global user.email "hi@serokell.io"
git config --global user.name "CI autodoc generator"
git remote remove auth-origin 2> /dev/null || :
git remote add auth-origin https://serokell:$AUTODOC_TOKEN@github.com/tqtezos/baseDAO.git
git fetch

our_branch="$BUILDKITE_BRANCH"
doc_branch="autodoc/$our_branch"
sha=$(git rev-parse --short HEAD)
git checkout origin/$doc_branch
git checkout -B $doc_branch
git merge -X theirs origin/$our_branch -m "Merge $our_branch"
mv tmp/*.md .
git add TrivialDAO.md GameDAO.md RegistryDAO.md TreasuryDAO.md
git commit --allow-empty -m "Documentation update for $sha"
git push --set-upstream auth-origin $doc_branch
git checkout @{-2}
