CFLAGS += -Wall -Wextra -Werror -fsanitize=address,undefined
LDFLAGS += -fsanitize=address,undefined

OMP_SRCS := omp.c loop.c task.c
SRCS := vec1.c vec2.c vec3.c vec4.c
SRCS += fib1.c fib2.c fib3.c
SRCS += $(OMP_SRCS)
OBJS := $(SRCS:.c=.o)
EXES := vec1 vec2 vec3 vec4
EXES += fib1 fib2 fib3

all: $(EXES)

vec1 vec2 fib1 fib2: CFLAGS += -fopenmp
vec1 vec2 fib1 fib2: LDFLAGS += -fopenmp
vec3 vec4 fib3: CFLAGS += -pthread
vec3 vec4 fib3: LDFLAGS += -pthread

vec2 vec3 vec4 fib2 fib3: libomp.a

libomp.a: $(OMP_SRCS:.c=.o)
	$(AR) rc $@ $^

test: lit.cfg.py $(EXES)
	@lit .

clean:
	$(RM) $(OBJS) $(EXES)
	$(RM) libomp.a depend.mk

depend.mk: $(SRCS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -MM $^ > $@

ifneq ($(MAKECMDGOALS),clean)
  -include depend.mk
endif

.PHONY: all test clean
