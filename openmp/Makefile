CC := gcc
CFLAGS += -Wall -Wextra -Werror -fsanitize=address,undefined
LDFLAGS += -fsanitize=address,undefined

SRCS := vec1.c vec2.c vec3.c omp.c
OBJS := $(SRCS:.c=.o)
BINS := vec1 vec2 vec3

all: $(BINS)

vec1 vec2: CFLAGS += -fopenmp
vec1 vec2: LDFLAGS += -fopenmp
vec3: CFLAGS += -pthread
vec3: LDFLAGS += -pthread

vec2 vec3: omp.o

check: all
	@set -e; \
	for vec in $(BINS); do \
		echo "Running $$vec:"; \
		./$$vec; \
		echo; \
	done

clean:
	rm -f depend $(OBJS) $(BINS)

depend: $(SRCS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -MM $^ > depend

ifneq ($(MAKECMDGOALS),clean)
  -include depend
endif

.PHONY: all check clean
