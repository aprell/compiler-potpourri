PROGS = test_basic test_cfg test_dom



PROGS_EXE = $(addsuffix .exe,$(PROGS))

all: $(PROGS_EXE)

$(PROGS_EXE):
	dune build $@

test:
	@echo "\n--- test_parse ----------"
	@./test_parse.sh
	@echo "\n--- test_utils ----------"
	@./test_utils.sh
	@echo

clean:
	dune clean
	rm -rf _test

.PHONY: all clean test
