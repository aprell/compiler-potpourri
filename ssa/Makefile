LIB := ssa

PROGS := test_ssa test/expect

all: $(LIB) $(PROGS) test

test: test/expect
	@for test in test pow fastpow fib; do \
		OUTPUT=test/$$test.out; \
		dune exec $<.exe "examples/$$test.hir" > $$OUTPUT.actual; \
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

.PHONY: all test

include ../utils/common.mk
