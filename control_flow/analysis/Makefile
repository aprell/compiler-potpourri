LIBS := control_flow
EXES := test_control_flow test/expect

all: test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh basic_blocks/examples

DOTS := $(wildcard examples/*.dot)
PNGS := $(DOTS:.dot=.png)

png: $(PNGS)

%.png: %.dot
	dot -Tpng $< > $@

%_cfg.dot %_domtree.dot: _build/default/test_control_flow.exe
	$< basic_blocks/$*.hir > /dev/null

.PHONY: all test png

include ../../utils/common.mk
