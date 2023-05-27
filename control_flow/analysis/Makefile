LIBS := control_flow
EXES := test_control_flow test/expect

all: test

test: test/expect
	@echo "Running expect tests"
	@../../utils/expect.sh basic_blocks/examples

HIRS := $(addprefix examples/,$(notdir $(wildcard basic_blocks/examples/*.hir)))
DOTS := $(patsubst %.hir,%_cfg.dot,$(HIRS))
DOTS += $(patsubst %.hir,%_domtree.dot,$(HIRS))
PNGS := $(DOTS:.dot=.png)

png: $(PNGS)

%.png: %.dot
	dot -Tpng $< > $@

%_cfg.dot %_domtree.dot: _build/default/test_control_flow.exe
	$< basic_blocks/$*.hir > /dev/null

.PHONY: all test png
.PRECIOUS: %_cfg.dot %_domtree.dot

include ../../utils/common.mk
