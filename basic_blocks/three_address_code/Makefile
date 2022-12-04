LIBS := three_address_code
EXES := test_IR test/expect

all: test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh

test-parse:
	@echo "\n--- test-parse ----------"
	@./test_parse.sh
	@echo

.PHONY: all test test-parse

include ../../utils/common.mk
