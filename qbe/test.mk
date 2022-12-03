CFLAGS := -Wall -Wextra
# Ignore conflicting types for pow
CFLAGS += -Wno-builtin-declaration-mismatch
CFLAGS += -fsanitize=address,undefined

test: a.out
	./$<

a.out: ../utils/test.c func.s
	$(CC) $(CFLAGS) $^

func.s: func.ssa
	qbe $< -o $@

func.ssa: _build/default/test_qbe.exe
	dune exec ./$(<F) > $@

clean:
	$(RM) a.out func.s func.ssa

.PHONY: test clean
