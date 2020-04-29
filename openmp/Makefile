CC := gcc
CFLAGS += -Wall -Wextra -Werror -fsanitize=address,undefined
LDFLAGS += -fsanitize=address,undefined

OMP_SRCS := omp.c loop.c task.c
SRCS := vec1.c vec2.c vec3.c vec4.c
SRCS += fib1.c fib2.c fib3.c
SRCS += $(OMP_SRCS)
OBJS := $(SRCS:.c=.o)
BINS := vec1 vec2 vec3 vec4
BINS += fib1 fib2 fib3

all: $(BINS)

vec1 vec2 fib1 fib2: CFLAGS += -fopenmp
vec1 vec2 fib1 fib2: LDFLAGS += -fopenmp
vec3 vec4 fib3: CFLAGS += -pthread
vec3 vec4 fib3: LDFLAGS += -pthread

vec2 vec3 vec4 fib2 fib3: libomp.a

libomp.a: $(OMP_SRCS:.c=.o)
	$(AR) rc $@ $^

check: all
	@set -e; \
	for vec in $(BINS); do \
		echo "Running $$vec:"; \
		./$$vec; \
		echo; \
	done

clean:
	rm -f depend libomp.a $(OBJS) $(BINS)

depend: $(SRCS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -MM $^ > depend

ifneq ($(MAKECMDGOALS),clean)
  -include depend
endif

.PHONY: all check clean
