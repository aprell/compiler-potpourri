LIB := basic_block

PROGS := test_basic_block test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@echo "Running expect tests"
	@../utils/expect.sh

test_utils:
	@echo "\n--- test_utils ----------"
	@./test_utils.sh
	@echo

.PHONY: all test test_utils

include ../utils/common.mk
