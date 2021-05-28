# SPDX-FileCopyrightText: 2021 TQ Tezos
# SPDX-License-Identifier: MPL-2.0
.PHONY: build test haddock haddock-no-deps stylish lint clean

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/
# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks
# Options for tests
STACK_DEV_TEST_OPTIONS = --fast --ghc-options -Wwarn
# Options for CI
STACK_CI_TEST_OPTIONS = --fast --ghc-options -Werror
# Addtional (specified by user) options passed to test executable
TEST_ARGUMENTS ?= ""
# Addtional (specified by user) options passed to `exec` command
EXEC_ARGUMENTS ?= ""
# Addtional (specified by user) options for specifying output file
# Mainly used when generating metadata
EXEC_OUTPUT ?= ""
# Packages to apply the command (build, test, e.t.c) for.
PACKAGE = baseDAO templateDAO

define call_test
	stack test $(PACKAGE) \
		--test-arguments "--color always $(TEST_ARGUMENTS) $1" $2
endef

# Build everything (including tests and benchmarks) with development options.
build:
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) $(PACKAGE)

test:
	$(call call_test,"",$(STACK_DEV_TEST_OPTIONS))

exec:
	stack exec -- $(PACKAGE) $(EXEC_ARGUMENTS) > "../$(EXEC_OUTPUT)"

# Like 'test' command, but enforces dumb terminal which may be useful to
# workardoung some issues with `tasty`.
# Primarily this one: https://github.com/feuerbach/tasty/issues/152
test-dumb-term:
	TERM=dumb $(call call_test,"",$(STACK_DEV_TEST_OPTIONS))

# Run tests with `--hide-successes` option. It forces dumb terminal,
# because otherwise this option is likely to work incorrectly.
test-hide-successes:
	TERM=dumb $(call call_test,"--hide-successes",$(STACK_DEV_TEST_OPTIONS))

# Run haddock for all packages.
haddock:
	stack haddock $(STACK_DEV_OPTIONS) $(PACKAGE)

# Run haddock for all our packages, but not for dependencies.
haddock-no-deps:
	stack haddock $(STACK_DEV_OPTIONS) $(PACKAGE) --no-haddock-deps

stylish:
	find . -name '.stack-work' -prune -o -name '*.hs' -exec stylish-haskell -i {} \;

lint:
	scripts/lint.sh

clean:
	stack clean $(PACKAGE)
