LIB := qbe

PROGS := test_qbe test/expect

all: $(LIB) $(PROGS)

test: all test/expect
	@$(MAKE) --no-print-directory -f test.mk
	@echo "Running expect tests"
	@test/expect.sh

.PHONY: all test

include ../utils/common.mk
