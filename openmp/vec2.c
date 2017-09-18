#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "omp.h"

static int *A;
static int *B;
static int *C;

struct omp_data {
    int n;
};

void main_omp_fn_0(void *omp_data)
{
    int n = ((struct omp_data *)omp_data)->n;
    int i;

    #pragma omp for schedule(static)
    for (i = 0; i < n; i++) {
        printf("%3d: T%d\n", i, omp_get_thread_num());
        C[i] = A[i] + B[i];
    }
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
