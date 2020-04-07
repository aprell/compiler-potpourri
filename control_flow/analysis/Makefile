LIB := control_flow

PROGS := test_cfg test_dom

all: $(LIB) $(PROGS)

%.png: %.dot
	dot -Tpng $< > $@

.PHONY: all

include ../../utils/common.mk
