LIB := three_address_code

PROGS := test_IR

all: $(LIB) $(PROGS)

test:
	@echo "\n--- test_parse ----------"
	@./test_parse.sh
	@echo

.PHONY: all test

include ../../utils/common.mk
