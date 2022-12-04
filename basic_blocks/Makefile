LIBS := basic_block
EXES := test_basic_block test/expect

all: test

test: test/expect
	@echo "Running expect tests"
	@../utils/expect.sh

test-utils:
	@echo "\n--- test-utils ----------"
	@./test_utils.sh
	@echo

.PHONY: all test test-utils

include ../utils/common.mk
