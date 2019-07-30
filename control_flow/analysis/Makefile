LIB := control_flow

PROGS := test_cfg test_dom



all: $(LIB) $(PROGS)

$(LIB):
	dune build $(addsuffix .a,$@)

$(PROGS):
	dune build $(addsuffix .exe,$@)

clean:
	dune clean

.PHONY: all clean
