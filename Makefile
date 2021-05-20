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


# Handle arguments of storage and config

# Storage
admin_address =
ifdef admin_address
admin_address_val = Some(\"$(admin_address)\" : address)
else
admin_address_val = (None : address option)
endif

guardian_address =
ifdef guardian_address
guardian_address_val = Some(\"$(guardian_address)\" : address)
else
guardian_address_val = (None : address option)
endif

governance_token_address =
ifdef governance_token_address
governance_token_address_val = Some(\"$(governance_token_address)\" : address)
else
governance_token_address_val = (None : address option)
endif

governance_token_id =
ifdef governance_token_id
governance_token_id_val = Some($(governance_token_id) : nat)
else
governance_token_id_val = (None : nat option)
endif

now_val =
ifdef now_val
now_val_option = Some($(now_val) : timestamp)
else
now_val_option = (None : timestamp option)
endif

metadata_map =
ifdef metadata_map
metadata_map_val = Some($(call escape_double_quote,$(metadata_map)) : metadata_map)
else
metadata_map_val = (None : metadata_map option)
endif

ledger =
ifdef ledger
ledger_val = Some($(call escape_double_quote,$(ledger)) : ledger_list)
else
ledger_val = (None : ledger_list option)
endif

# Config
max_quorum =
ifdef max_quorum
max_quorum_val = Some($(max_quorum) : nat)
else
max_quorum_val = (None : nat option)
endif

min_quorum =
ifdef min_quorum
min_quorum_val = Some($(min_quorum) : nat)
else
min_quorum_val = (None : nat option)
endif

quorum_threshold =
ifdef quorum_threshold
quorum_threshold_val = Some($(quorum_threshold) : nat)
else
quorum_threshold_val = (None : nat option)
endif

voting_period =
ifdef voting_period
voting_period_val = Some($(voting_period) : nat)
else
voting_period_val = (None : nat option)
endif

proposal_flush_time =
ifdef proposal_flush_time
proposal_flush_time_val = Some($(proposal_flush_time) : nat)
else
proposal_flush_time_val = (None : nat option)
endif

proposal_expired_time =
ifdef proposal_expired_time
proposal_expired_time_val = Some($(proposal_expired_time) : nat)
else
proposal_expired_time_val = (None : nat option)
endif

fixed_proposal_fee_in_token =
ifdef fixed_proposal_fee_in_token
fixed_proposal_fee_in_token_val = Some($(fixed_proposal_fee_in_token) : nat)
else
fixed_proposal_fee_in_token_val = (None : nat option)
endif

max_quorum_change =
ifdef max_quorum_change
max_quorum_change_val = Some($(max_quorum_change) : nat)
else
max_quorum_change_val = (None : nat option)
endif

quorum_change =
ifdef quorum_change
quorum_change_val = Some($(quorum_change) : nat)
else
quorum_change_val = (None : nat option)
endif

governance_total_supply =
ifdef governance_total_supply
governance_total_supply_val = Some($(governance_total_supply) : nat)
else
governance_total_supply_val = (None : nat option)
endif


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

$(OUT)/trivialDAO_storage.tz: src/**
	# ============== Compiling TrivialDAO storage ============== #
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/trivialDAO_storage.tz \
      src/base_DAO.mligo base_DAO_contract "default_full_storage( \
        { storage_data = \
          { admin = $(admin_address_val) \
          ; guardian = $(guardian_address_val) \
					; governance_token_address = $(governance_token_address_val) \
					; governance_token_id = $(governance_token_id_val) \
					; now_val = $(now_val_option) \
					; metadata_map = $(metadata_map_val) \
					; ledger_lst = $(ledger_val) \
          } \
        ; config_data = \
          { max_quorum = $(max_quorum_val) \
          ; min_quorum = $(min_quorum_val) \
          ; voting_period = $(voting_period_val) \
          ; proposal_flush_time = $(proposal_flush_time_val) \
          ; proposal_expired_time = $(proposal_expired_time_val) \
          ; quorum_threshold = $(quorum_threshold_val) \
          ; fixed_proposal_fee_in_token = $(fixed_proposal_fee_in_token_val) \
          ; quorum_change = $(quorum_change_val) \
          ; max_quorum_change = $(max_quorum_change_val) \
          ; governance_total_supply = $(governance_total_supply_val) \
          } \
        })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/trivialDAO_storage.tz" for compilation result  #
	#

# $(OUT)/registryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
# $(OUT)/registryDAO_storage.tz : guardian_address = KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh
# $(OUT)/registryDAO_storage.tz : governance_token_address = KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5
# $(OUT)/registryDAO_storage.tz : governance_token_id = 0n
# $(OUT)/registryDAO_storage.tz : frozen_scale_value = 1n
# $(OUT)/registryDAO_storage.tz : frozen_extra_value = 0n
# $(OUT)/registryDAO_storage.tz : max_proposal_size = 100n
# $(OUT)/registryDAO_storage.tz : slash_scale_value = 1n
# $(OUT)/registryDAO_storage.tz : slash_division_value = 0n
# $(OUT)/registryDAO_storage.tz : min_xtz_amount = 0mutez
# $(OUT)/registryDAO_storage.tz : max_xtz_amount = 100mutez
# $(OUT)/registryDAO_storage.tz : now_val = `date +"%s"`
# $(OUT)/registryDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
# $(OUT)/registryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
# $(OUT)/registryDAO_storage.tz : ledger = ([] : ledger_list)
# $(OUT)/registryDAO_storage.tz : quorum_threshold = 10n
# $(OUT)/registryDAO_storage.tz : min_quorum = 1n
# $(OUT)/registryDAO_storage.tz : max_quorum = 99n

# $(OUT)/registryDAO_storage.tz : voting_period = 950400n # 11 days
# $(OUT)/registryDAO_storage.tz : quorum_change = 5n
# $(OUT)/registryDAO_storage.tz : max_quorum_change = 19n
# $(OUT)/registryDAO_storage.tz : proposal_flush_time = 1900801n # 22 days 1 seconds
# $(OUT)/registryDAO_storage.tz : proposal_expired_time = 2851200n # 33 days
# $(OUT)/registryDAO_storage.tz : governance_total_supply = 100n
# $(OUT)/registryDAO_storage.tz: src/**
# 	# ============== Compiling RegistryDAO storage ============== #
# 	mkdir -p $(OUT)
# 	$(BUILD_STORAGE) --output-file $(OUT)/registryDAO_storage.tz \
#       src/registryDAO.mligo base_DAO_contract "default_registry_DAO_full_storage( \
#         { base_data = \
#           { storage_data = \
#             { admin = (\"$(admin_address)\" : address) \
#             ; guardian = (\"$(guardian_address)\" : address) \
#             ; governance_token = \
#               { address = (\"$(governance_token_address)\" : address) \
#               ; token_id = $(governance_token_id) \
#               } \
#             ; now_val = ($(now_val) : timestamp)  \
#             ; metadata_map = $(call escape_double_quote,$(metadata_map)) \
#             ; ledger_lst = $(call escape_double_quote,$(ledger)) \
#             } \
#           ; config_data = \
#             { max_quorum = { numerator = ($(max_quorum) * quorum_denominator)/100n } \
#             ; min_quorum = { numerator = ($(min_quorum) * quorum_denominator)/100n } \
#             ; voting_period = { length = $(voting_period) } \
#             ; proposal_flush_time = $(proposal_flush_time) \
#             ; proposal_expired_time = $(proposal_expired_time) \
#             ; fixed_proposal_fee_in_token = $(fixed_proposal_fee_in_token) \
#             ; quorum_threshold = { numerator = ($(quorum_threshold) * quorum_denominator)/100n } \
#             ; quorum_change = { numerator = ($(quorum_change) * quorum_denominator)/100n } \
#             ; max_quorum_change = { numerator = ($(max_quorum_change) * quorum_denominator)/100n } \
#             ; governance_total_supply = $(governance_total_supply) \
#             }} \
#           ; frozen_scale_value = $(frozen_scale_value) \
#           ; frozen_extra_value = $(frozen_extra_value) \
#           ; max_proposal_size = $(max_proposal_size) \
#           ; slash_scale_value = $(slash_scale_value) \
#           ; slash_division_value = $(slash_division_value) \
#           ; min_xtz_amount = $(min_xtz_amount) \
#           ; max_xtz_amount = $(max_xtz_amount) \
#           })"
# 	# ================= Compilation successful ================= #
# 	# See "$(OUT)/registryDAO_storage.tz" for compilation result #
# 	#

# $(OUT)/treasuryDAO_storage.tz : admin_address = tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af
# $(OUT)/treasuryDAO_storage.tz : guardian_address = KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh
# $(OUT)/treasuryDAO_storage.tz : governance_token_address = KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5
# $(OUT)/treasuryDAO_storage.tz : governance_token_id = 0n
# $(OUT)/treasuryDAO_storage.tz : frozen_scale_value = 0n
# $(OUT)/treasuryDAO_storage.tz : frozen_extra_value = 0n
# $(OUT)/treasuryDAO_storage.tz : max_proposal_size = 0n
# $(OUT)/treasuryDAO_storage.tz : slash_scale_value = 0n
# $(OUT)/treasuryDAO_storage.tz : slash_division_value = 0n
# $(OUT)/treasuryDAO_storage.tz : min_xtz_amount = 0mutez
# $(OUT)/treasuryDAO_storage.tz : max_xtz_amount = 100mutez
# $(OUT)/treasuryDAO_storage.tz : now_val = `date +"%s"`
# $(OUT)/treasuryDAO_storage.tz : metadata_map = (Big_map.empty : metadata_map)
# $(OUT)/treasuryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
# $(OUT)/treasuryDAO_storage.tz : ledger = ([] : ledger_list)
# $(OUT)/treasuryDAO_storage.tz : quorum_threshold = 10n
# $(OUT)/treasuryDAO_storage.tz : min_quorum = 1n
# $(OUT)/treasuryDAO_storage.tz : max_quorum = 99n
# $(OUT)/treasuryDAO_storage.tz : voting_period = 950400n # 11 days
# $(OUT)/treasuryDAO_storage.tz : quorum_change = 5n
# $(OUT)/treasuryDAO_storage.tz : max_quorum_change = 19n
# $(OUT)/treasuryDAO_storage.tz : proposal_flush_time = 1900801n # 22 days 1 seconds
# $(OUT)/treasuryDAO_storage.tz : proposal_expired_time = 2851200n # 33 days
# $(OUT)/treasuryDAO_storage.tz : governance_total_supply = 100n
# $(OUT)/treasuryDAO_storage.tz: src/**
# 	# ============== Compiling TreasuryDAO storage ============== #
# 	mkdir -p $(OUT)
# 	$(BUILD_STORAGE) --output-file $(OUT)/treasuryDAO_storage.tz \
#        src/treasuryDAO.mligo base_DAO_contract "default_treasury_DAO_full_storage( \
#         { base_data = \
#           { storage_data = \
#             { admin = (\"$(admin_address)\" : address) \
#             ; guardian = (\"$(guardian_address)\" : address) \
#             ; governance_token = \
#               { address = (\"$(governance_token_address)\" : address) \
#               ; token_id =  $(governance_token_id) \
#               } \
#             ; now_val = ($(now_val) : timestamp)  \
#             ; metadata_map = $(call escape_double_quote,$(metadata_map)) \
#             ; ledger_lst = $(call escape_double_quote,$(ledger)) \
#             } \
#           ; config_data = \
#             { max_quorum = { numerator = ($(max_quorum) * quorum_denominator)/100n } \
#             ; min_quorum = { numerator = ($(min_quorum) * quorum_denominator)/100n } \
#             ; voting_period = { length = $(voting_period) } \
#             ; proposal_flush_time = $(proposal_flush_time) \
#             ; proposal_expired_time = $(proposal_expired_time) \
#             ; fixed_proposal_fee_in_token = $(fixed_proposal_fee_in_token) \
#             ; quorum_threshold = { numerator = ($(quorum_threshold) * quorum_denominator)/100n } \
#             ; quorum_change = { numerator = ($(quorum_change) * quorum_denominator)/100n } \
#             ; max_quorum_change = { numerator = ($(max_quorum_change) * quorum_denominator)/100n } \
#             ; governance_total_supply = $(governance_total_supply) \
#             }} \
#           ; frozen_scale_value = $(frozen_scale_value) \
#           ; frozen_extra_value = $(frozen_extra_value) \
#           ; max_proposal_size = $(max_proposal_size) \
#           ; slash_scale_value = $(slash_scale_value) \
#           ; slash_division_value = $(slash_division_value) \
#           ; min_xtz_amount = $(min_xtz_amount) \
#           ; max_xtz_amount = $(max_xtz_amount) \
#           })"

# 	# ============== Compilation successful ============== #
# 	# See "$(OUT)/treasuryDAO_storage.tz" for compilation result #
# 	#

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
	$(MAKE) -C haskell test PACKAGE=baseDAO-ligo-meta \
	BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
	REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz \
	TREASURY_STORAGE_PATH=../$(OUT)/treasuryDAO_storage.tz

typescript: all
	$(MAKE) -C haskell build PACKAGE=baseDAO-ligo-meta \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn" \
	BASEDAO_LIGO_PATH=../$(OUT)/baseDAO.tz \
	REGISTRY_STORAGE_PATH=../$(OUT)/registryDAO_storage.tz \
	TREASURY_STORAGE_PATH=../$(OUT)/treasuryDAO_storage.tz
	rm -rf $(TS_OUT)/baseDAO/src/generated/*
	stack exec -- baseDAO-ligo-meta generate-typescript --target=$(TS_OUT)/baseDAO/src/generated/

clean:
	rm -rf $(OUT)
