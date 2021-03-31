<!--
SPDX-FileCopyrightText: 2021 TQ Tezos

SPDX-License-Identifier: LicenseRef-MIT-TQ
-->

# Contribution Guidelines

## Reporting Issues

Please [open an issue](https://github.com/tqtezos/baseDAO/issues/new)
if you find a bug or have a feature request.
Before submitting a bug report or feature request, check to make sure it hasn't already been submitted.

The more detailed your report is, the faster it can be resolved.
Please use issue templates to create an issue.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve our project, merge requests are most welcome.

Our merge request template contains a [checklist](/.github/pull_request_template.md#white_check_mark-checklist-for-your-pull-request) of acceptance criteria for your merge request.
Please read it before you start contributing and make sure your contributions adhere to this checklist.

### Prelude

All Haskell code uses
[Universum](https://hackage.haskell.org/package/universum) as a
replacement for the default prelude.

### Tests

We use [`tasty`](https://hackage.haskell.org/package/tasty) as our primary top-level testing framework.
Some old code may use `hspec` instead, but all new code must use `tasty`.
We use [`tasty-discover`](https://hackage.haskell.org/package/tasty-discover) to automatically find all tests.
We still require explicit exports to ensure that we don't accidentally miss some test.
If we accidentally name some test in a way which will be ignored by `tasty-discover`, `weeder` will detect a useless export.

Some hints regarding `tasty` and our test-suite:
1. You can use `--hide-successes` to see only failing tests.
It's useful because otherwise if test suite fails you need to find the cause of failure manually.
2. However, beware of [this issue](https://github.com/feuerbach/tasty/issues/152) with `--hide-successes`.
In short, this option is somewhat broken when `tasty` thinks that it outputs to console.
A workaround is to set `TERM=dumb`.
3. You can run tests using our `Makefile`, see below.

## Branching policy

This project uses a variation of the [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow) branching model with two branches. Naming of long-lived branches is different:
* `develop` branch from OneFlow is called `master` in this repository.
* `master` branch from OneFlow is called `production` in this repository.

So code in the `master` branch represents latest development version and code in the `production` branch represents latest stable version.

## Legal

We want to make sure that our projects come with correct licensing information
and that this information is machine-readable, thus we are following the
[REUSE Practices][reuse] – feel free to click the link and read about them,
but, basically, it all boils down to the following:

  * Add the following header at the very top (but below the shebang, if there
    is one) of each source file in the repository (yes, each and every source
    file – it is not as hard as it might sound):

    ```haskell
    -- SPDX-FileCopyrightText: 2021 TQ Tezos
    --
    -- SPDX-License-Identifier: LicenseRef-MIT-TQ
    ```

    (This is an example for Haskell; adapt it as needed for other languages.)

    The license identifier should be the same as the one in the `LICENSE` file.

  * If you are copying any source files from some other project, and they do not
    contain a header with a copyright and a machine-readable license identifier,
    add it, but be extra careful and make sure that information you are recording
    is correct.

    If the license of the file is different from the one used in the project and
    you do not plan to relicense it, use the appropriate license identifier and
    make sure the license text exists in the `LICENSES` directory.

    If the file contains the entire license in its header, it is best to move the
    text to a separate file in the `LICENSES` directory and leave a reference.

  * If you are copying pieces of code from some other project, leave a note in the
    comments, stating where you copied it from, who is the copyright owner, and
    what license applies.

  * All the same rules apply to documentation that is stored in the repository.

These simple rules should cover most of situation you are likely to encounter.
In case of doubt, consult the [REUSE Practices][reuse] document.

[reuse]: https://reuse.software/spec/
