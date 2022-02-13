CFLAGS := -Wall -Wextra
# Ignore conflicting types for pow
CFLAGS += -Wno-builtin-declaration-mismatch
CFLAGS += -fsanitize=address,undefined

test: a.out
	./$<

a.out: main.c func.s
	$(CC) $(CFLAGS) $^

func.s: func.ssa
	qbe $< -o $@

func.ssa: _build/default/test_qbe.exe
	dune exec ./$(<F) > $@

clean:
	rm -f a.out func.s func.ssa

.PHONY: test clean
