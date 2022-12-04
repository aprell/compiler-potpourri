LIBS := qbe
EXES := test_qbe test/expect

all: test

test: test/expect
	@$(MAKE) --no-print-directory -f test.mk
	@echo "Running expect tests"
	@test/expect.sh

.PHONY: all test

include ../utils/common.mk
