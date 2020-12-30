# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# Ligo executable
LIGO ?= ligo

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Where to put build files
OUT ?= out

.PHONY: all clean test

all: $(OUT)/baseDAO.tz

# Compile LIGO contract into its michelson representation.
$(OUT)/baseDAO.tz: src
	mkdir -p $(OUT)
	# ============== Compiling contract ============== #
	$(BUILD) src/base_DAO.mligo base_DAO_contract --output-file $(OUT)/baseDAO.tz
	# ============== Compilation successful ============== #
	# See "$(OUT)/baseDAO.tz" for compilation result #

	# strip the surrounding braces and indentation,
	# note that dollar char is escaped as part of Makefile
	sed -i '/^ *$$/d' $(OUT)/baseDAO.tz
	sed -i 's/^[{ ] //g' $(OUT)/baseDAO.tz
	sed -i '$$s/[}] *$$//' $(OUT)/baseDAO.tz

test: $(OUT)/baseDAO.tz
	$(MAKE) -C .. test PACKAGE=baseDAO-ligo-tests \
    BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz

clean:
	rm -rf $(OUT)
