LIB := control_flow

PROGS := test_control_flow test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh basic_blocks/examples

png: $(wildcard examples/*.dot)
	@$(MAKE) $(^:.dot=.png)

%.png: %.dot
	dot -Tpng $< > $@

.PHONY: all test png

include ../../utils/common.mk
