# SPDX-FileCopyrightText: 2021 Tezos Commons
# SPDX-License-Identifier: LicenseRef-MIT-TC

# Ligo executable
LIGO ?= ligo

# Morley executable used for contract optimization
MORLEY ?= morley

# Env variable to determine whether the resulting contract should
# be optimized via morley
OPTIMIZE ?= false

# Compile code
BUILD = $(LIGO) compile contract --syntax cameligo

# Compile storage
BUILD_STORAGE = $(LIGO) compile storage --syntax cameligo

# Compile parameter
BUILD_PARAMETER = $(LIGO) compile parameter --syntax cameligo

# Utility function to escape single quotes
escape_quote = $(subst ','\'',$(1))
# Utility function to escape double quotes
escape_double_quote = $(subst $\",$\\",$(1))
# Utility function to require a variable to be defined
require_defined = $(if $(value $1),$(value $1),$(error "$1 needs to be defined"))

# Where to put build files
OUT ?= out
# Where to put typescript files
TS_OUT ?= typescript

.PHONY: all clean test typescript

all: $(OUT)/lambdatreasuryDAO.tz  $(OUT)/lambdaregistryDAO.tz $(OUT)/trivialDAO.tz $(OUT)/registryDAO.tz $(OUT)/treasuryDAO.tz $(OUT)/lambdaDAO.tz $(OUT)/plist_contract.tz

# Compile LIGO contract into its michelson representation.
$(OUT)/%DAO.tz: src/**/**
	cp src/variants/$*/implementation.mligo src/implementation.mligo
	cp src/variants/$*/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	# ============== Compiling contract ============== #
	$(BUILD) src/base_DAO.mligo -e base_DAO_contract --output-file $@
	# ============== Compilation successful ============== #
	# See "$@" for compilation result #

	# strip the surrounding braces and indentation,
	# note that dollar char is escaped as part of Makefile
	sed -i '/^ *$$/d' $@
	sed -i 's/^[{ ] //g' $@
	sed -i '$$s/[}] *$$//' $@
ifeq ($(OPTIMIZE), true)
	# ============== Optimizing contract ============== #
	$(MORLEY) optimize --contract $@ --output $@
endif

$(OUT)/trivialDAO_storage.tz : metadata_map = Big_map.empty
$(OUT)/trivialDAO_storage.tz : freeze_history = []
$(OUT)/trivialDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/trivialDAO_storage.tz : quorum_threshold = 10n
$(OUT)/trivialDAO_storage.tz : min_quorum = 1n
$(OUT)/trivialDAO_storage.tz : max_quorum = 99n
$(OUT)/trivialDAO_storage.tz : period = 15840n
$(OUT)/trivialDAO_storage.tz : quorum_change = 5n
$(OUT)/trivialDAO_storage.tz : max_quorum_change = 19n
$(OUT)/trivialDAO_storage.tz : governance_total_supply = 1000n
$(OUT)/trivialDAO_storage.tz : proposal_flush_level = 36000n
$(OUT)/trivialDAO_storage.tz : proposal_expired_level = 47520n
$(OUT)/trivialDAO_storage.tz: src/**
	# ============== Compiling TrivialDAO storage ============== #
	cp src/variants/trivial/implementation.mligo src/implementation.mligo
	cp src/variants/trivial/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/trivialDAO_storage.tz \
      src/base_DAO.mligo -e base_DAO_contract "default_storage( \
        { admin = (\"$(call require_defined,admin_address)\" : address) \
				; guardian = (\"$(call require_defined,guardian_address)\" : address) \
				; governance_token = \
					{ address = (\"$(call require_defined,governance_token_address)\" : address) \
					; token_id = ($(call require_defined,governance_token_id) : nat) \
					} \
				; start_level = {blocks = $(call require_defined,start_level)} \
				; metadata_map = ($(call escape_double_quote,$(metadata_map)) : metadata_map) \
				; freeze_history = ($(call escape_double_quote,$(freeze_history)) : freeze_history_list) \
        ; config_data = \
          { max_quorum = { numerator = (($(max_quorum) : nat) * quorum_denominator)/100n } \
          ; min_quorum = { numerator = (($(min_quorum) : nat) * quorum_denominator)/100n } \
          ; period = { blocks = ($(period) : nat) } \
          ; proposal_flush_level = { blocks = ($(proposal_flush_level) : nat) } \
          ; proposal_expired_level = { blocks = ($(proposal_expired_level) : nat) }\
          ; fixed_proposal_fee_in_token = ($(fixed_proposal_fee_in_token) : nat) \
          ; quorum_threshold = { numerator = (($(quorum_threshold) : nat) * quorum_denominator)/100n } \
          ; quorum_change = { numerator = (($(quorum_change) : nat) * quorum_denominator)/100n } \
          ; max_quorum_change = { numerator = (($(max_quorum_change) : nat) * quorum_denominator)/100n } \
          ; governance_total_supply = ($(governance_total_supply) : nat) \
          } \
        })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/trivialDAO_storage.tz" for compilation result	#

$(OUT)/registryDAO_storage.tz : frozen_scale_value = 1n
$(OUT)/registryDAO_storage.tz : frozen_extra_value = 0n
$(OUT)/registryDAO_storage.tz : max_proposal_size = 100n
$(OUT)/registryDAO_storage.tz : slash_scale_value = 1n
$(OUT)/registryDAO_storage.tz : slash_division_value = 1n
$(OUT)/registryDAO_storage.tz : min_xtz_amount = 0mutez
$(OUT)/registryDAO_storage.tz : max_xtz_amount = 100mutez
$(OUT)/registryDAO_storage.tz : metadata_map = Big_map.empty
$(OUT)/registryDAO_storage.tz : freeze_history = []
$(OUT)/registryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/registryDAO_storage.tz : quorum_threshold = 10n
$(OUT)/registryDAO_storage.tz : min_quorum = 1n
$(OUT)/registryDAO_storage.tz : max_quorum = 99n
$(OUT)/registryDAO_storage.tz : period = 15840n
$(OUT)/registryDAO_storage.tz : quorum_change = 5n
$(OUT)/registryDAO_storage.tz : max_quorum_change = 19n
$(OUT)/registryDAO_storage.tz : proposal_flush_level = 36000n
$(OUT)/registryDAO_storage.tz : proposal_expired_level = 47520n
$(OUT)/registryDAO_storage.tz : governance_total_supply = 1000n
$(OUT)/registryDAO_storage.tz: src/**
	# ============== Compiling RegistryDAO storage ============== #
	cp src/variants/registry/implementation.mligo src/implementation.mligo
	cp src/variants/registry/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/registryDAO_storage.tz \
      src/base_DAO.mligo -e base_DAO_contract "default_registry_DAO_storage( \
        { base_data = \
          { admin = (\"$(call require_defined,admin_address)\" : address) \
					; guardian = (\"$(call require_defined,guardian_address)\" : address) \
					; governance_token = \
						{ address = (\"$(call require_defined,governance_token_address)\" : address) \
						; token_id = ($(call require_defined,governance_token_id) : nat) \
						} \
					; start_level = {blocks = $(call require_defined,start_level)} \
					; metadata_map = ($(call escape_double_quote,$(metadata_map)) : metadata_map) \
					; freeze_history = ($(call escape_double_quote,$(freeze_history)) : freeze_history_list) \
          ; config_data = \
            { max_quorum = { numerator = (($(max_quorum) : nat) * quorum_denominator)/100n } \
            ; min_quorum = { numerator = (($(min_quorum) : nat) * quorum_denominator)/100n } \
            ; period = { blocks = ($(period) : nat) } \
            ; proposal_flush_level = { blocks = ($(proposal_flush_level) : nat) } \
            ; proposal_expired_level = { blocks = ($(proposal_expired_level) : nat) } \
            ; fixed_proposal_fee_in_token = ($(fixed_proposal_fee_in_token) : nat) \
            ; quorum_threshold = { numerator = (($(quorum_threshold) : nat) * quorum_denominator)/100n } \
            ; quorum_change = { numerator = (($(quorum_change) : nat) * quorum_denominator)/100n } \
            ; max_quorum_change = { numerator = (($(max_quorum_change) : nat) * quorum_denominator)/100n } \
            ; governance_total_supply = ($(governance_total_supply) : nat) \
            }} \
          ; frozen_scale_value = ($(frozen_scale_value) : nat) \
          ; frozen_extra_value = ($(frozen_extra_value) : nat) \
          ; max_proposal_size = ($(max_proposal_size) : nat) \
          ; slash_scale_value = ($(slash_scale_value) : nat) \
          ; slash_division_value = ($(slash_division_value) : nat) \
          ; min_xtz_amount = ($(min_xtz_amount) : tez) \
          ; max_xtz_amount = ($(max_xtz_amount) : tez) \
          })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/registryDAO_storage.tz" for compilation result #

$(OUT)/treasuryDAO_storage.tz : frozen_scale_value = 1n
$(OUT)/treasuryDAO_storage.tz : frozen_extra_value = 0n
$(OUT)/treasuryDAO_storage.tz : max_proposal_size = 100n
$(OUT)/treasuryDAO_storage.tz : slash_scale_value = 1n
$(OUT)/treasuryDAO_storage.tz : slash_division_value = 1n
$(OUT)/treasuryDAO_storage.tz : min_xtz_amount = 0mutez
$(OUT)/treasuryDAO_storage.tz : max_xtz_amount = 100mutez
$(OUT)/treasuryDAO_storage.tz : metadata_map = Big_map.empty
$(OUT)/treasuryDAO_storage.tz : freeze_history = []
$(OUT)/treasuryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/treasuryDAO_storage.tz : quorum_threshold = 10n
$(OUT)/treasuryDAO_storage.tz : min_quorum = 1n
$(OUT)/treasuryDAO_storage.tz : max_quorum = 99n
$(OUT)/treasuryDAO_storage.tz : period = 15840n
$(OUT)/treasuryDAO_storage.tz : quorum_change = 5n
$(OUT)/treasuryDAO_storage.tz : max_quorum_change = 19n
$(OUT)/treasuryDAO_storage.tz : proposal_flush_level = 36000n
$(OUT)/treasuryDAO_storage.tz : proposal_expired_level = 47520n
$(OUT)/treasuryDAO_storage.tz : governance_total_supply = 1000n
$(OUT)/treasuryDAO_storage.tz: src/**
	# ============== Compiling TreasuryDAO storage ============== #
	cp src/variants/treasury/implementation.mligo src/implementation.mligo
	cp src/variants/treasury/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/treasuryDAO_storage.tz \
       src/base_DAO.mligo -e base_DAO_contract "default_treasury_DAO_storage( \
        { base_data = \
          { admin = (\"$(call require_defined,admin_address)\" : address) \
					; guardian = (\"$(call require_defined,guardian_address)\" : address) \
					; governance_token = \
						{ address = (\"$(call require_defined,governance_token_address)\" : address) \
						; token_id = ($(call require_defined,governance_token_id) : nat) \
						} \
					; start_level = {blocks = $(call require_defined,start_level)} \
					; metadata_map = ($(call escape_double_quote,$(metadata_map)) : metadata_map) \
					; freeze_history = ($(call escape_double_quote,$(freeze_history)) : freeze_history_list) \
          ; config_data = \
            { max_quorum = { numerator = (($(max_quorum) : nat) * quorum_denominator)/100n } \
            ; min_quorum = { numerator = (($(min_quorum) : nat) * quorum_denominator)/100n } \
            ; period = { blocks = ($(period) : nat) } \
            ; proposal_flush_level = { blocks = ($(proposal_flush_level) : nat) } \
            ; proposal_expired_level = { blocks = ($(proposal_expired_level) : nat) } \
            ; fixed_proposal_fee_in_token = ($(fixed_proposal_fee_in_token) : nat) \
            ; quorum_threshold = { numerator = (($(quorum_threshold) : nat) * quorum_denominator)/100n } \
            ; quorum_change = { numerator = (($(quorum_change) : nat) * quorum_denominator)/100n } \
            ; max_quorum_change = { numerator = (($(max_quorum_change) : nat) * quorum_denominator)/100n } \
            ; governance_total_supply = ($(governance_total_supply) : nat) \
            }} \
          ; frozen_scale_value = ($(frozen_scale_value) : nat) \
          ; frozen_extra_value = ($(frozen_extra_value) : nat) \
          ; max_proposal_size = ($(max_proposal_size) : nat) \
          ; slash_scale_value = ($(slash_scale_value) : nat) \
          ; slash_division_value = ($(slash_division_value) : nat) \
          ; min_xtz_amount = ($(min_xtz_amount) : tez) \
          ; max_xtz_amount = ($(max_xtz_amount) : tez) \
          })"

	# ============== Compilation successful ============== #
	# See "$(OUT)/treasuryDAO_storage.tz" for compilation result #
	#

$(OUT)/lambdaDAO_storage.tz : metadata_map = Big_map.empty
$(OUT)/lambdaDAO_storage.tz : freeze_history = []
$(OUT)/lambdaDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/lambdaDAO_storage.tz : quorum_threshold = 10n
$(OUT)/lambdaDAO_storage.tz : min_quorum = 1n
$(OUT)/lambdaDAO_storage.tz : max_quorum = 99n
$(OUT)/lambdaDAO_storage.tz : period = 15840n
$(OUT)/lambdaDAO_storage.tz : quorum_change = 5n
$(OUT)/lambdaDAO_storage.tz : max_quorum_change = 19n
$(OUT)/lambdaDAO_storage.tz : governance_total_supply = 1000n
$(OUT)/lambdaDAO_storage.tz : proposal_flush_level = 36000n
$(OUT)/lambdaDAO_storage.tz : proposal_expired_level = 47520n
$(OUT)/lambdaDAO_storage.tz: src/**
	# ============== Compiling LambdaDAO storage ============== #
	cp src/variants/lambda/implementation.mligo src/implementation.mligo
	cp src/variants/lambda/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/lambdaDAO_storage.tz \
      src/base_DAO.mligo -e base_DAO_contract "default_storage( \
        { admin = (\"$(call require_defined,admin_address)\" : address) \
				; guardian = (\"$(call require_defined,guardian_address)\" : address) \
				; governance_token = \
					{ address = (\"$(call require_defined,governance_token_address)\" : address) \
					; token_id = ($(call require_defined,governance_token_id) : nat) \
					} \
				; start_level = {blocks = $(call require_defined,start_level)} \
				; metadata_map = ($(call escape_double_quote,$(metadata_map)) : metadata_map) \
				; freeze_history = ($(call escape_double_quote,$(freeze_history)) : freeze_history_list) \
        ; config_data = \
          { max_quorum = { numerator = (($(max_quorum) : nat) * quorum_denominator)/100n } \
          ; min_quorum = { numerator = (($(min_quorum) : nat) * quorum_denominator)/100n } \
          ; period = { blocks = ($(period) : nat) } \
          ; proposal_flush_level = { blocks = ($(proposal_flush_level) : nat) } \
          ; proposal_expired_level = { blocks = ($(proposal_expired_level) : nat) }\
          ; fixed_proposal_fee_in_token = ($(fixed_proposal_fee_in_token) : nat) \
          ; quorum_threshold = { numerator = (($(quorum_threshold) : nat) * quorum_denominator)/100n } \
          ; quorum_change = { numerator = (($(quorum_change) : nat) * quorum_denominator)/100n } \
          ; max_quorum_change = { numerator = (($(max_quorum_change) : nat) * quorum_denominator)/100n } \
          ; governance_total_supply = ($(governance_total_supply) : nat) \
          } \
        })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/lambdaDAO_storage.tz" for compilation result	#
	#
$(OUT)/lambdaregistryDAO_storage.tz : metadata_map = Big_map.empty
$(OUT)/lambdaregistryDAO_storage.tz : freeze_history = []
$(OUT)/lambdaregistryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/lambdaregistryDAO_storage.tz : quorum_threshold = 10n
$(OUT)/lambdaregistryDAO_storage.tz : min_quorum = 1n
$(OUT)/lambdaregistryDAO_storage.tz : max_quorum = 99n
$(OUT)/lambdaregistryDAO_storage.tz : period = 15840n
$(OUT)/lambdaregistryDAO_storage.tz : quorum_change = 5n
$(OUT)/lambdaregistryDAO_storage.tz : max_quorum_change = 19n
$(OUT)/lambdaregistryDAO_storage.tz : governance_total_supply = 1000n
$(OUT)/lambdaregistryDAO_storage.tz : proposal_flush_level = 36000n
$(OUT)/lambdaregistryDAO_storage.tz : proposal_expired_level = 47520n
$(OUT)/lambdaregistryDAO_storage.tz: src/**
	# ============== Compiling LambdaDAO storage ============== #
	cp src/variants/lambdaregistry/implementation.mligo src/implementation.mligo
	cp src/variants/lambdaregistry/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/lambdaregistryDAO_storage.tz \
      src/base_DAO.mligo -e base_DAO_contract "default_storage( \
        { admin = (\"$(call require_defined,admin_address)\" : address) \
				; guardian = (\"$(call require_defined,guardian_address)\" : address) \
				; governance_token = \
					{ address = (\"$(call require_defined,governance_token_address)\" : address) \
					; token_id = ($(call require_defined,governance_token_id) : nat) \
					} \
				; start_level = {blocks = $(call require_defined,start_level)} \
				; metadata_map = ($(call escape_double_quote,$(metadata_map)) : metadata_map) \
				; freeze_history = ($(call escape_double_quote,$(freeze_history)) : freeze_history_list) \
        ; config_data = \
          { max_quorum = { numerator = (($(max_quorum) : nat) * quorum_denominator)/100n } \
          ; min_quorum = { numerator = (($(min_quorum) : nat) * quorum_denominator)/100n } \
          ; period = { blocks = ($(period) : nat) } \
          ; proposal_flush_level = { blocks = ($(proposal_flush_level) : nat) } \
          ; proposal_expired_level = { blocks = ($(proposal_expired_level) : nat) }\
          ; fixed_proposal_fee_in_token = ($(fixed_proposal_fee_in_token) : nat) \
          ; quorum_threshold = { numerator = (($(quorum_threshold) : nat) * quorum_denominator)/100n } \
          ; quorum_change = { numerator = (($(quorum_change) : nat) * quorum_denominator)/100n } \
          ; max_quorum_change = { numerator = (($(max_quorum_change) : nat) * quorum_denominator)/100n } \
          ; governance_total_supply = ($(governance_total_supply) : nat) \
          } \
        })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/lambdaregistryDAO_storage.tz" for compilation result	#
	#
	#
$(OUT)/lambdatreasuryDAO_storage.tz : metadata_map = Big_map.empty
$(OUT)/lambdatreasuryDAO_storage.tz : freeze_history = []
$(OUT)/lambdatreasuryDAO_storage.tz : fixed_proposal_fee_in_token = 0n
$(OUT)/lambdatreasuryDAO_storage.tz : quorum_threshold = 10n
$(OUT)/lambdatreasuryDAO_storage.tz : min_quorum = 1n
$(OUT)/lambdatreasuryDAO_storage.tz : max_quorum = 99n
$(OUT)/lambdatreasuryDAO_storage.tz : period = 15840n
$(OUT)/lambdatreasuryDAO_storage.tz : quorum_change = 5n
$(OUT)/lambdatreasuryDAO_storage.tz : max_quorum_change = 19n
$(OUT)/lambdatreasuryDAO_storage.tz : governance_total_supply = 1000n
$(OUT)/lambdatreasuryDAO_storage.tz : proposal_flush_level = 36000n
$(OUT)/lambdatreasuryDAO_storage.tz : proposal_expired_level = 47520n
$(OUT)/lambdatreasuryDAO_storage.tz: src/**
	# ============== Compiling LambdaDAO storage ============== #
	cp src/variants/lambdatreasury/implementation.mligo src/implementation.mligo
	cp src/variants/lambdatreasury/storage.mligo src/implementation_storage.mligo
	mkdir -p $(OUT)
	$(BUILD_STORAGE) --output-file $(OUT)/lambdatreasuryDAO_storage.tz \
      src/base_DAO.mligo -e base_DAO_contract "default_storage( \
        { admin = (\"$(call require_defined,admin_address)\" : address) \
				; guardian = (\"$(call require_defined,guardian_address)\" : address) \
				; governance_token = \
					{ address = (\"$(call require_defined,governance_token_address)\" : address) \
					; token_id = ($(call require_defined,governance_token_id) : nat) \
					} \
				; start_level = {blocks = $(call require_defined,start_level)} \
				; metadata_map = ($(call escape_double_quote,$(metadata_map)) : metadata_map) \
				; freeze_history = ($(call escape_double_quote,$(freeze_history)) : freeze_history_list) \
        ; config_data = \
          { max_quorum = { numerator = (($(max_quorum) : nat) * quorum_denominator)/100n } \
          ; min_quorum = { numerator = (($(min_quorum) : nat) * quorum_denominator)/100n } \
          ; period = { blocks = ($(period) : nat) } \
          ; proposal_flush_level = { blocks = ($(proposal_flush_level) : nat) } \
          ; proposal_expired_level = { blocks = ($(proposal_expired_level) : nat) }\
          ; fixed_proposal_fee_in_token = ($(fixed_proposal_fee_in_token) : nat) \
          ; quorum_threshold = { numerator = (($(quorum_threshold) : nat) * quorum_denominator)/100n } \
          ; quorum_change = { numerator = (($(quorum_change) : nat) * quorum_denominator)/100n } \
          ; max_quorum_change = { numerator = (($(max_quorum_change) : nat) * quorum_denominator)/100n } \
          ; governance_total_supply = ($(governance_total_supply) : nat) \
          } \
        })"
	# ================= Compilation successful ================= #
	# See "$(OUT)/lambdatreasuryDAO_storage.tz" for compilation result	#
	#
# For development and testing purpose
test-storage:
	make $(OUT)/trivialDAO_storage.tz \
		admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
		guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
		governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
		governance_token_id=0n \
		start_level=100n

	make $(OUT)/treasuryDAO_storage.tz \
		admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
		guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
		governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
		governance_token_id=0n \
		start_level=100n

	make $(OUT)/registryDAO_storage.tz \
		admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
		guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
		governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
		governance_token_id=0n \
		start_level=100n

	make $(OUT)/lambdaDAO_storage.tz \
		admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
		guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
		governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
		governance_token_id=0n \
		start_level=100n

	make $(OUT)/lambdaregistryDAO_storage.tz \
		admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
		guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
		governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
		governance_token_id=0n \
		start_level=100n

	make $(OUT)/lambdatreasuryDAO_storage.tz \
		admin_address=tz1QozfhaUW4wLnohDo6yiBUmh7cPCSXE9Af \
		guardian_address=KT1QbdJ7M7uAQZwLpvzerUyk7LYkJWDL7eDh \
		governance_token_address=KT1RdwP8XJPjFyGoUsXFQnQo1yNm6gUqVdp5 \
		governance_token_id=0n \
		start_level=100n
haskell-resources: all test-storage
	# ============== Copying resources for Haskell library ============== #
	mkdir -p haskell/resources
	cp $(OUT)/* haskell/resources

metadata : frozen_token_symbol = frozen_token
metadata : frozen_token_name = Frozen Token
metadata : frozen_token_thumbnail_uri =
metadata : output = metadata.json
metadata: haskell-resources
	$(MAKE) -C haskell build PACKAGE=baseDAO-ligo-meta \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"

	$(MAKE) -C haskell exec PACKAGE=baseDAO-ligo-meta \
		EXEC_ARGUMENTS="print-metadata \
		--frozen-token-symbol $(frozen_token_symbol) \
		--frozen-token-name \"$(call escape_double_quote,$(frozen_token_name))\" \
		--frozen-token-thumbnail-uri \"$(call escape_double_quote,$(frozen_token_thumbnail_uri))\" \
		" EXEC_OUTPUT=$(output)

test: haskell-resources
	$(MAKE) -C haskell test PACKAGE=baseDAO-ligo-meta

typescript: haskell-resources
	$(MAKE) -C haskell build PACKAGE=baseDAO-ligo-meta \
		STACK_DEV_OPTIONS="--fast --ghc-options -Wwarn"
	rm -rf $(TS_OUT)/baseDAO/src/generated/*
	stack exec -- baseDAO-ligo-meta generate-typescript --target=$(TS_OUT)/baseDAO/src/generated/

$(OUT)/plist_contract.tz: haskell/test/Test/Plist/**
	mkdir -p $(OUT)
	# ============== Compiling contract ============== #
	$(BUILD) haskell/test/Test/Plist/plist_contract.mligo -e plist_contract --output-file $(OUT)/plist_contract.tz
	# ============== Compilation successful ============== #
	# See "$(OUT)/plist_contract.tz" for compilation result #

	# strip the surrounding braces and indentation,
	# note that dollar char is escaped as part of Makefile
	sed -i '/^ *$$/d' $(OUT)/plist_contract.tz
	sed -i 's/^[{ ] //g' $(OUT)/plist_contract.tz
	sed -i '$$s/[}] *$$//' $(OUT)/plist_contract.tz


clean:
	rm -rf $(OUT)
	rm -rf haskell/resources

stylish:
	find haskell/ -name '*.hs' -exec stylish-haskell -i '{}' \;
