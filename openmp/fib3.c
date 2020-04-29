#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "omp.h"

int fib(int);

struct fib_task_args {
    struct task_frame *frame;
    int n;
    int *x;
};

void fib_task_func(void *args)
{
    assert(args != NULL);

    int n = ((struct fib_task_args *)args)->n;
    int *x = ((struct fib_task_args *)args)->x;

    *x = fib(n);
}

int fib(int n)
{
    if (n < 2) return n;

    struct task_frame frame = {0};
    int x, y;

    omp_task_enqueue(fib, &frame, n - 1, &x);

    y = fib(n - 2);

    omp_taskwait(&frame);

    return x + y;
}

struct omp_data {
    int n;
};

void main_omp_fn_0(void *omp_data)
{
    int n = ((struct omp_data *)omp_data)->n;

    // Here we cheat a little and introduce a top-level task to avoid having to
    // roll our own tasking-compatible barrier
    static struct task_frame frame = {1};

    if (omp_get_thread_num() == 0) {
        printf("fib(%d) = %d\n", n, fib(n));
        // Signal end of tasking
        __sync_sub_and_fetch(&frame.count, 1);
    } else {
        // Keep executing tasks while waiting for the completion of the
        // top-level task
        omp_taskwait(&frame);
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
