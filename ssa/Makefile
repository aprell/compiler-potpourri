LIBS := ssa analysis optim
EXES := test_ssa test_optim test_sscp test_sccp test_reachability
EXES += test/expect analysis/test/expect

all: test

test: test/expect analysis/test/expect
	@echo "Running expect tests"
	@../utils/expect.sh
	@cd analysis && ../../utils/expect.sh ../examples

.PHONY: all test

include ../utils/common.mk
