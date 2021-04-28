CC := clang
CFLAGS := -Wall -Wextra -Wno-override-module -fsanitize=undefined

test: a.out
	./$<

a.out: main.ll fib.ll
	$(CC) $(CFLAGS) $^

fib.ll: _build/default/test_llvm.exe
	dune exec ./$(<F) > $@

clean:
	rm -f a.out fib.ll

.PHONY: test clean
