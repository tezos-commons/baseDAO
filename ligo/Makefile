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

# Compile parameter
BUILD_PARAMETER = $(LIGO) compile-parameter --syntax cameligo

# Where to put build files
OUT ?= out

.PHONY: all clean test

all: \
	$(OUT)/baseDAO.tz \
	$(OUT)/trivialDAO_storage.tz \
	$(OUT)/registryDAO_storage.tz \
	$(OUT)/treasuryDAO_storage.tz \
	$(OUT)/entrypoints/accept_ownership.tz \
	$(OUT)/entrypoints/balance_of.tz \
	$(OUT)/entrypoints/burn.tz \
	$(OUT)/entrypoints/confirm_migration.tz \
	$(OUT)/entrypoints/drop_proposal.tz \
	$(OUT)/entrypoints/flush.tz \
	$(OUT)/entrypoints/get_vote_permit_counter.tz \
	$(OUT)/entrypoints/migrate.tz \
	$(OUT)/entrypoints/mint.tz \
	$(OUT)/entrypoints/propose.tz \
	$(OUT)/entrypoints/set_quorum_threshold.tz \
	$(OUT)/entrypoints/set_voting_period.tz \
	$(OUT)/entrypoints/transfer.tz \
	$(OUT)/entrypoints/transfer_contract_tokens.tz \
	$(OUT)/entrypoints/transfer_ownership.tz \
	$(OUT)/entrypoints/update_operators.tz \
	$(OUT)/entrypoints/vote.tz

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
	#

$(OUT)/entrypoints/%.tz: src/**
	# ==== Compiling parameter for storable entrypoint ==== #
	mkdir -p $(OUT)/entrypoints
	$(BUILD_PARAMETER) --output-file $(OUT)/entrypoints/$*.tz src/startup.mligo startup_contract 'make_ep_param("$*", storable_$*)'
	# ============== Compilation successful =============== #
	# See "$(OUT)/entrypoints/$*.tz" for compilation result #
	#

$(OUT)/trivialDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/trivialDAO_storage.tz : token_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/trivialDAO_storage.tz: src/**
	# ============== Compiling TrivialDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/trivialDAO_storage.tz src/base_DAO.mligo base_DAO_contract 'default_full_storage(("$(admin_address)": address), ("$(token_address)": address))'
	# ================= Compilation successful ================= #
	# See "$(OUT)/trivialDAO_storage.tz" for compilation result  #
	#

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
	# ================= Compilation successful ================= #
	# See "$(OUT)/registryDAO_storage.tz" for compilation result #
	#

$(OUT)/treasuryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/treasuryDAO_storage.tz : token_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/treasuryDAO_storage.tz : a = 0n
$(OUT)/treasuryDAO_storage.tz : b = 0n
$(OUT)/treasuryDAO_storage.tz : s_max = 0n
$(OUT)/treasuryDAO_storage.tz : c = 0n
$(OUT)/treasuryDAO_storage.tz : d = 0n
$(OUT)/treasuryDAO_storage.tz : y = 0mutez
$(OUT)/treasuryDAO_storage.tz : z = 100mutez
$(OUT)/treasuryDAO_storage.tz: src/**
	# ============== Compiling TreasuryDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/treasuryDAO_storage.tz src/treasuryDAO.mligo base_DAO_contract 'default_treasury_DAO_full_storage(("$(admin_address)": address), ("$(token_address)": address), (${a}, $(b), $(s_max), $(c), $(d), $(y), $(z)))'
	# ============== Compilation successful ============== #
	# See "$(OUT)/treasuryDAO_storage.tz" for compilation result #
	#

test: all
	$(MAKE) -C .. test PACKAGE=baseDAO-ligo-meta \
    BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
    BASEDAO_LIGO_EPS_DIR_PATH=../$(OUT)/entrypoints/ \
    REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz \
    TREASURY_STORAGE_PATH=../$(OUT)/treasuryDAO_storage.tz

clean:
	rm -rf $(OUT)
