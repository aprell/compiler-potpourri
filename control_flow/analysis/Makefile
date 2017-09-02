PROGS = test_basic



PROGS_BYTE = $(addsuffix .byte,$(PROGS))
PROGS_NATIVE = $(addsuffix .native,$(PROGS))

default: $(PROGS_BYTE)

all: default $(PROGS_NATIVE)

$(PROGS_BYTE) $(PROGS_NATIVE):
	ocamlbuild -use-menhir $@

clean:
	ocamlbuild -clean
	rm -rf _test

# Let ocamlbuild handle the dependencies
.PHONY: $(PROGS_BYTE) $(PROGS_NATIVE)
.PHONY: default all clean
