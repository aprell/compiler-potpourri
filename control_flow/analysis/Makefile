LIB := control_flow

PROGS := test_cfg test_dom test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh basic_blocks/examples

%.png: %.dot
	dot -Tpng $< > $@

.PHONY: all test

include ../../utils/common.mk
