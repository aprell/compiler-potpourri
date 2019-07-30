$(LIB):
	dune build $(addsuffix .a,$@)

$(PROGS):
	dune build $(addsuffix .exe,$@)

clean:
	dune clean
	rm -rf _test

.PHONY: clean
