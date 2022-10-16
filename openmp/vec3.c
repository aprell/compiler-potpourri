#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "omp.h"

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

static int *A;
static int *B;
static int *C;

struct omp_data {
    int n;
};

void main_omp_fn_0(void *omp_data)
{
    int n = ((struct omp_data *)omp_data)->n;
    int from, to;

    omp_work_share_init(0, n, 1);
    //omp_barrier();

    if (omp_split_static(&from, &to)) {
        for (int i = from; i < to; i++) {
            printf("Thread %d: %3d\n", omp_get_thread_num(), i);
            C[i] = A[i] + B[i];
        }
    }

    omp_barrier();
    omp_work_share_destroy();
}

int main(int argc, char *argv[])
{
    int n = argc > 1 ? atoi(argv[1]) : 10;
    int i;

    A = malloc(n * sizeof(int));
    B = malloc(n * sizeof(int));
    C = malloc(n * sizeof(int));
    assert(A && B && C);

    for (i = 0; i < n; i++) {
        A[i] = i;
        B[i] = i+1;
    }

    struct omp_data omp_data;
    omp_data.n = n;

    omp_parallel_start(main_omp_fn_0, &omp_data, 0);

    main_omp_fn_0(&omp_data);

    omp_parallel_end();

    for (i = 0; i < n; i++) {
        assert(C[i] == 2*i + 1);
    }

    free(A);
    free(B);
    free(C);

    return 0;
}

// RUN: ./vec3 10 | FileCheck %s
//
// CHECK-DAG: Thread 0:   0
// CHECK-DAG: Thread 0:   1
// CHECK-DAG: Thread 0:   2
// CHECK-DAG: Thread 1:   3
// CHECK-DAG: Thread 1:   4
// CHECK-DAG: Thread 1:   5
// CHECK-DAG: Thread 2:   6
// CHECK-DAG: Thread 2:   7
// CHECK-DAG: Thread 3:   8
// CHECK-DAG: Thread 3:   9
