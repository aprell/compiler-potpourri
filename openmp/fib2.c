#include <stdio.h>
#include <stdlib.h>
#include "omp.h"

int fib(int n)
{
    if (n < 2) return n;

    int x, y;

    #pragma omp task shared(x)
    x = fib(n - 1);

    y = fib(n - 2);

    #pragma omp taskwait

    return x + y;
}

struct omp_data {
    int n;
};

void main_omp_fn_0(void *omp_data)
{
    int n = ((struct omp_data *)omp_data)->n;

    if (omp_get_thread_num() == 0) {
        printf("fib(%d) = %d\n", n, fib(n));
    }

    omp_barrier();
}

int main(int argc, char *argv[])
{
    int n = argc > 1 ? atoi(argv[1]) : 20;

    struct omp_data omp_data;

    omp_data.n = n;

    omp_parallel_start(main_omp_fn_0, &omp_data, 0);

    main_omp_fn_0(&omp_data);

    omp_parallel_end();

    return 0;
}
