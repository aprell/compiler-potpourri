LIBS := data_flow dominators liveness
EXES := test/expect

all: test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh

.PHONY: all test

include ../../utils/common.mk
