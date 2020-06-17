LIB := three_address_code

PROGS := test_IR test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@echo "Running expect tests"; \
	for test in test pow fastpow fib; do \
		OUTPUT=test/$$test.ir; \
		dune exec --no-print-directory $<.exe "examples/$$test.hir" > $$OUTPUT.actual; \
		git diff --no-index $$OUTPUT.expect $$OUTPUT.actual; \
		if [ $$? -eq 0 ]; then \
			rm $$OUTPUT.actual; \
		else \
			echo; \
			read -p "Accept this output? (y/n) " -n1; \
			echo; \
			if [[ $$REPLY =~ [Yy] ]]; then \
				mv $$OUTPUT.actual $$OUTPUT.expect; \
			fi \
		fi \
	done

test_parse:
	@echo "\n--- test_parse ----------"
	@./test_parse.sh
	@echo

.PHONY: all test test_parse

include ../../utils/common.mk
