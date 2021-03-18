# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# Ligo executable
LIGO ?= ligo

# Morley executable used for contract optimization
MORLEY ?= morley

# Morley tool executable for the origination of large contracts
LARGE_ORIGINATOR ?= morley-large-originator

# Env variable to determine whether the resulting contract should
# be optimized via morley
OPTIMIZE ?= false

# Compile code
BUILD = $(LIGO) compile-contract --syntax cameligo

# Compile storage
BUILD_STORAGE = $(LIGO) compile-storage --syntax cameligo

# Compile parameter
BUILD_PARAMETER = $(LIGO) compile-parameter --syntax cameligo

# Originate large contract (morley-client based)
ORIGINATE ?= $(LARGE_ORIGINATOR) originate

# Originate steps to build a large contract by other means
ORIGINATE_STEPS ?= $(LARGE_ORIGINATOR) steps

# Utility function to escape single quotes
escape_quote = $(subst ','\'',$(1))

# Where to put build files
OUT ?= out

.PHONY: all clean test

all: \
	$(OUT)/baseDAO.tz \
	$(OUT)/trivialDAO_storage.tz \
	$(OUT)/registryDAO_storage.tz \
	$(OUT)/treasuryDAO_storage.tz

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

$(OUT)/trivialDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/trivialDAO_storage.tz : token_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/trivialDAO_storage.tz : now_val = Tezos.now
$(OUT)/trivialDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/trivialDAO_storage.tz: src/**
	# ============== Compiling TrivialDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/trivialDAO_storage.tz src/base_DAO.mligo base_DAO_contract 'default_full_storage(("$(admin_address)": address), ("$(token_address)": address), $(now_val), $(metadata_map))'
	# ================= Compilation successful ================= #
	# See "$(OUT)/trivialDAO_storage.tz" for compilation result  #
	#

$(OUT)/registryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/registryDAO_storage.tz : token_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/registryDAO_storage.tz : frozen_scale_value = 1n
$(OUT)/registryDAO_storage.tz : frozen_extra_value = 0n
$(OUT)/registryDAO_storage.tz : max_proposal_size = 100n
$(OUT)/registryDAO_storage.tz : slash_scale_value = 1n
$(OUT)/registryDAO_storage.tz : slash_division_value = 0n
$(OUT)/registryDAO_storage.tz : now_val = Tezos.now
$(OUT)/registryDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/registryDAO_storage.tz: src/**
	# ============== Compiling RegistryDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/registryDAO_storage.tz src/registryDAO.mligo base_DAO_contract 'default_registry_DAO_full_storage(("$(admin_address)": address), ("$(token_address)": address), ${frozen_scale_value}, $(frozen_extra_value), $(max_proposal_size), $(slash_scale_value), $(slash_division_value), $(now_val), $(metadata_map))'
	# ================= Compilation successful ================= #
	# See "$(OUT)/registryDAO_storage.tz" for compilation result #
	#

$(OUT)/treasuryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/treasuryDAO_storage.tz : token_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/treasuryDAO_storage.tz : frozen_scale_value = 0n
$(OUT)/treasuryDAO_storage.tz : frozen_extra_value = 0n
$(OUT)/treasuryDAO_storage.tz : max_proposal_size = 0n
$(OUT)/treasuryDAO_storage.tz : slash_scale_value = 0n
$(OUT)/treasuryDAO_storage.tz : slash_division_value = 0n
$(OUT)/treasuryDAO_storage.tz : min_xtz_amount = 0mutez
$(OUT)/treasuryDAO_storage.tz : max_xtz_amount = 100mutez
$(OUT)/treasuryDAO_storage.tz : now_val = Tezos.now
$(OUT)/treasuryDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/treasuryDAO_storage.tz: src/**
	# ============== Compiling TreasuryDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/treasuryDAO_storage.tz src/treasuryDAO.mligo base_DAO_contract 'default_treasury_DAO_full_storage(("$(admin_address)": address), ("$(token_address)": address), (${frozen_scale_value}, $(frozen_extra_value), $(max_proposal_size), $(slash_scale_value), $(slash_division_value), $(min_xtz_amount), $(max_xtz_amount)), $(now_val), $(metadata_map))'
	# ============== Compilation successful ============== #
	# See "$(OUT)/treasuryDAO_storage.tz" for compilation result #
	#

originate : storage = $(OUT)/trivialDAO_storage.tz
originate : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
originate : contract_name = baseDAO
originate: $(OUT)/baseDAO.tz
	# ============== Originating DAO with $(storage) ============== #
	@$(ORIGINATE) --contract $(OUT)/baseDAO.tz --from "$(admin_address)" \
								--initial-storage '$(call escape_quote,$(shell cat $(storage)))' \
								 --contract-name "$(contract_name)"
	# =================== Origination completed =================== #
	#

originate-steps : storage = $(OUT)/trivialDAO_storage.tz
originate-steps : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
originate-steps : destination = $(OUT)/steps
originate-steps: $(OUT)/baseDAO.tz
	# ============== Originating DAO steps with $(storage) ============== #
	@$(ORIGINATE_STEPS) --contract $(OUT)/baseDAO.tz --from "$(admin_address)" \
								--initial-storage '$(call escape_quote,$(shell cat $(storage)))' \
								 --destination "$(destination)"
	# ================== Steps saved in $(destination) ================== #
	#

test: all
	$(MAKE) -C .. test PACKAGE=baseDAO-ligo-meta \
    BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
    REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz \
    TREASURY_STORAGE_PATH=../$(OUT)/treasuryDAO_storage.tz

clean:
	rm -rf $(OUT)
