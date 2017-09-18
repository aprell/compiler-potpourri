CC = gcc
CFLAGS = -Wall -Wextra -Werror -fsanitize=address,undefined

all: vec1 vec2 vec3

vec1: CFLAGS += -fopenmp
vec2: CFLAGS += -fopenmp
vec3: CFLAGS += -pthread

vec2 vec3: omp.c

check:
	@set -e; \
	for vec in vec1 vec2 vec3; do \
		echo "Running $$vec:"; \
		./$$vec; \
		echo; \
	done

clean:
	rm -f vec1 vec2 vec3

.PHONY: all check clean
