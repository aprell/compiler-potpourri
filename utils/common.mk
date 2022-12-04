all: $(LIBS) $(EXES)

$(LIBS):
	dune build $(addsuffix .a,$@)

$(EXES):
	dune build $(addsuffix .exe,$@)

clean:
	dune clean
	rm -rf _test

.PHONY: all clean
