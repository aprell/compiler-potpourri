LIB := ssa optim

PROGS := test_ssa test_sscp test_sccp test_reachability
PROGS += test/expect analysis/test/expect

all: $(LIB) $(PROGS) test

test: test/expect analysis/test/expect
	@echo "Running expect tests"
	@../utils/expect.sh
	@cd analysis && ../../utils/expect.sh ../examples

.PHONY: all test

include ../utils/common.mk
