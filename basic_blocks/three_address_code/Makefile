LIB := three_address_code



all: $(LIB)

$(LIB):
	dune build $(addsuffix .a,$@)

test:
	@echo "\n--- test_parse ----------"
	@./test_parse.sh
	@echo

clean:
	dune clean
	rm -rf _test

.PHONY: all clean test
