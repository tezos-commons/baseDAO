# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# Ligo executable
LIGO ?= ligo

# Morley executable used for contract optimization
MORLEY ?= morley

# Env variable to determine whether the resulting contract should
# be optimized via morley
OPTIMIZE ?= false

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Compile storage
BUILD_STORAGE = $(LIGO) compile-storage --syntax cameligo

# Where to put build files
OUT ?= out

.PHONY: all clean test

all: $(OUT)/baseDAO.tz $(OUT)/registryDAO_storage.tz

# Compile LIGO contract into its michelson representation.
$(OUT)/baseDAO.tz: src/**
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
ifeq ($(OPTIMIZE), true)
	# ============== Optimizing contract ============== #
	$(MORLEY) optimize --contract $(OUT)/baseDAO.tz --output $(OUT)/baseDAO.tz
endif


$(OUT)/registryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/registryDAO_storage.tz : token_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/registryDAO_storage.tz : a = 1n
$(OUT)/registryDAO_storage.tz : b = 0n
$(OUT)/registryDAO_storage.tz : s_max = 100n
$(OUT)/registryDAO_storage.tz : c = 1n
$(OUT)/registryDAO_storage.tz : d = 0n
$(OUT)/registryDAO_storage.tz: src/**

	# ============== Compiling RegistryDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/registryDAO_storage.tz src/registryDAO.mligo base_DAO_contract 'default_registry_DAO_full_storage(("$(admin_address)": address), ("$(token_address)": address), ${a}, $(b), $(s_max), $(c), $(d))'
	# ============== Compilation successful ============== #
	# See "$(OUT)/registryDAO_storage.tz" for compilation result #

test: $(OUT)/baseDAO.tz $(OUT)/registryDAO_storage.tz
	$(MAKE) -C .. test PACKAGE=baseDAO-ligo-meta \
    BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
    REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz

clean:
	rm -rf $(OUT)
