LIB := llvm

PROGS := test_llvm

all: $(LIB) $(PROGS)

test: all
	@$(MAKE) --no-print-directory -f test.mk

.PHONY: all test

include ../utils/common.mk
