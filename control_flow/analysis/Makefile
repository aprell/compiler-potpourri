LIBS := control_flow
EXES := test_control_flow test/expect

all: test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh basic_blocks/examples

png: $(wildcard examples/*.dot)
	@$(MAKE) $(^:.dot=.png)

%.png: %.dot
	dot -Tpng $< > $@

.PHONY: all test png

include ../../utils/common.mk
