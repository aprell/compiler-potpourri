LIB := three_address_code

PROGS := test_IR test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh

test_parse:
	@echo "\n--- test_parse ----------"
	@./test_parse.sh
	@echo

.PHONY: all test test_parse

include ../../utils/common.mk
