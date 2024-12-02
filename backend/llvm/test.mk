CC := clang-16
CFLAGS := -Wall -Wextra -Wno-override-module
# Ignore incompatible redeclaration of pow
CFLAGS += -Wno-incompatible-library-redeclaration
CFLAGS += -fsanitize=address,undefined

test: a.out
	./$<

a.out: ../../utils/test.c func.ll
	$(CC) $(CFLAGS) $^

func.ll: _build/default/test_llvm.exe
	dune exec ./$(<F) > $@

clean:
	$(RM) a.out func.ll

.PHONY: test clean
