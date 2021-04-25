CC := clang
CFLAGS := -Wall -Wextra -Wno-override-module -fsanitize=undefined

test: a.out
	@./$<

a.out: main.ll func.ll
	$(CC) $(CFLAGS) $^

clean:
	rm -f a.out

.PHONY: test clean
