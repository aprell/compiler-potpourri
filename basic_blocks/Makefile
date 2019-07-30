LIB := basic

PROGS := test_basic



all: $(LIB) $(PROGS)

$(LIB):
	dune build $(addsuffix .a,$@)

$(PROGS):
	dune build $(addsuffix .exe,$@)

test:
	@echo "\n--- test_utils ----------"
	@./test_utils.sh
	@echo

clean:
	dune clean
	rm -rf _test

.PHONY: all clean test
