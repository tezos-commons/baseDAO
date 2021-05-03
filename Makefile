# SPDX-FileCopyrightText: 2021 TQ Tezos
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
# Utility function to escape double quotes
escape_double_quote = $(subst $\",$\\",$(1))

# Where to put build files
OUT ?= out
# Where to put typescript files
TS_OUT ?= typescript

.PHONY: all clean test typescript

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
$(OUT)/trivialDAO_storage.tz : governance_token_address = KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5
$(OUT)/trivialDAO_storage.tz : governance_token_id = 0n
$(OUT)/trivialDAO_storage.tz : now_val = Tezos.now
$(OUT)/trivialDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/trivialDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/trivialDAO_storage.tz : ledger = ([] : ledger_list)
$(OUT)/trivialDAO_storage.tz : quorum_threshold = {numerator = 1n; denominator = 10n}
$(OUT)/trivialDAO_storage.tz : voting_period = 950400n # 11 days
$(OUT)/trivialDAO_storage.tz: src/**
	# ============== Compiling TrivialDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/trivialDAO_storage.tz \
      src/base_DAO.mligo base_DAO_contract "default_full_storage( \
        { storage_data = \
          { admin = (\"$(admin_address)\" : address) \
          ; governance_token = \
            { address = (\"$(governance_token_address)\" : address) \
            ; token_id = $(governance_token_id) \
            } \
          ; now_val = $(now_val) \
          ; metadata_map = $(call escape_double_quote,$(metadata_map)) \
          ; ledger_lst = $(call escape_double_quote,$(ledger)) \
          ; quorum_threshold = $(quorum_threshold) \
          ; voting_period = $(voting_period) \
          } \
        ; config_data = \
          { max_quorum = {numerator = 99n; denominator = 100n} \
          ; min_quorum = {numerator = 1n; denominator = 100n} \
          ; max_period = 2592000n \
          ; min_period = 1n \
          } \
        })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/trivialDAO_storage.tz" for compilation result  #
	#

$(OUT)/registryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/registryDAO_storage.tz : governance_token_address = KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5
$(OUT)/registryDAO_storage.tz : governance_token_id = 0n
$(OUT)/registryDAO_storage.tz : frozen_scale_value = 1n
$(OUT)/registryDAO_storage.tz : frozen_extra_value = 0n
$(OUT)/registryDAO_storage.tz : max_proposal_size = 100n
$(OUT)/registryDAO_storage.tz : slash_scale_value = 1n
$(OUT)/registryDAO_storage.tz : slash_division_value = 0n
$(OUT)/registryDAO_storage.tz : min_xtz_amount = 0mutez
$(OUT)/registryDAO_storage.tz : max_xtz_amount = 100mutez
$(OUT)/registryDAO_storage.tz : now_val = Tezos.now
$(OUT)/registryDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/registryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/registryDAO_storage.tz : ledger = ([] : ledger_list)
$(OUT)/registryDAO_storage.tz : quorum_threshold = {numerator = 1n; denominator = 10n}
$(OUT)/registryDAO_storage.tz : voting_period = 950400n # 11 days
$(OUT)/registryDAO_storage.tz: src/**
	# ============== Compiling RegistryDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/registryDAO_storage.tz \
      src/registryDAO.mligo base_DAO_contract "default_registry_DAO_full_storage( \
        { base_data = \
          { storage_data = \
            { admin = (\"$(admin_address)\" : address) \
            ; governance_token = \
              { address = (\"$(governance_token_address)\" : address) \
              ; token_id = $(governance_token_id) \
              } \
            ; now_val = $(now_val) \
            ; metadata_map = $(call escape_double_quote,$(metadata_map)) \
            ; ledger_lst = $(call escape_double_quote,$(ledger)) \
            ; quorum_threshold = $(quorum_threshold) \
            ; voting_period = $(voting_period) \
            } \
          ; config_data = \
            { max_quorum = {numerator = 99n; denominator = 100n} \
            ; min_quorum = {numerator = 1n; denominator = 100n} \
            ; max_period = 2592000n \
            ; min_period = 1n} \
            } \
          ; frozen_scale_value = $(frozen_scale_value) \
          ; frozen_extra_value = $(frozen_extra_value) \
          ; max_proposal_size = $(max_proposal_size) \
          ; slash_scale_value = $(slash_scale_value) \
          ; slash_division_value = $(slash_division_value) \
          ; min_xtz_amount = $(min_xtz_amount) \
          ; max_xtz_amount = $(max_xtz_amount) \
          })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/registryDAO_storage.tz" for compilation result #
	#

$(OUT)/treasuryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
$(OUT)/treasuryDAO_storage.tz : governance_token_address = KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5
$(OUT)/treasuryDAO_storage.tz : governance_token_id = 0n
$(OUT)/treasuryDAO_storage.tz : frozen_scale_value = 0n
$(OUT)/treasuryDAO_storage.tz : frozen_extra_value = 0n
$(OUT)/treasuryDAO_storage.tz : max_proposal_size = 0n
$(OUT)/treasuryDAO_storage.tz : slash_scale_value = 0n
$(OUT)/treasuryDAO_storage.tz : slash_division_value = 0n
$(OUT)/treasuryDAO_storage.tz : min_xtz_amount = 0mutez
$(OUT)/treasuryDAO_storage.tz : max_xtz_amount = 100mutez
$(OUT)/treasuryDAO_storage.tz : now_val = Tezos.now
$(OUT)/treasuryDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
$(OUT)/treasuryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/treasuryDAO_storage.tz : ledger = ([] : ledger_list)
$(OUT)/treasuryDAO_storage.tz : quorum_threshold = {numerator = 1n; denominator = 10n}
$(OUT)/treasuryDAO_storage.tz : voting_period = 950400n # 11 days
$(OUT)/treasuryDAO_storage.tz: src/**
	# ============== Compiling TreasuryDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/treasuryDAO_storage.tz \
       src/treasuryDAO.mligo base_DAO_contract "default_treasury_DAO_full_storage( \
        { base_data = \
          { storage_data = \
            { admin = (\"$(admin_address)\" : address) \
            ; governance_token = \
              { address = (\"$(governance_token_address)\" : address) \
              ; token_id =  $(governance_token_id) \
              } \
            ; now_val = $(now_val) \
            ; metadata_map = $(call escape_double_quote,$(metadata_map)) \
            ; ledger_lst = $(call escape_double_quote,$(ledger)) \
            ; quorum_threshold = $(quorum_threshold) \
            ; voting_period = $(voting_period) \
            } \
          ; config_data = \
            { max_quorum = {numerator = 99n; denominator = 100n} \
            ; min_quorum = {numerator = 1n; denominator = 100n} \
            ; max_period = 2592000n \
            ; min_period = 1n} \
            } \
          ; frozen_scale_value = $(frozen_scale_value) \
          ; frozen_extra_value = $(frozen_extra_value) \
          ; max_proposal_size = $(max_proposal_size) \
          ; slash_scale_value = $(slash_scale_value) \
          ; slash_division_value = $(slash_division_value) \
          ; min_xtz_amount = $(min_xtz_amount) \
          ; max_xtz_amount = $(max_xtz_amount) \
          })"

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
	$(MAKE) -C lorentz test PACKAGE=baseDAO-ligo-meta \
	BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
	REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz \
	TREASURY_STORAGE_PATH=../$(OUT)/treasuryDAO_storage.tz

typescript: all
	$(MAKE) -C lorentz build PACKAGE=baseDAO-ligo-meta \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn" \
	BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
	REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz \
	TREASURY_STORAGE_PATH=../$(OUT)/treasuryDAO_storage.tz
	rm -rf $(TS_OUT)/baseDAO/src/generated/*
	stack exec -- baseDAO-ligo-meta generate-typescript --target=$(TS_OUT)/baseDAO/src/generated/

clean:
	rm -rf $(OUT)
