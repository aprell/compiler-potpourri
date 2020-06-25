LIB := data_flow liveness

PROGS := test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh

.PHONY: all test

include ../../utils/common.mk
