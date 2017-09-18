#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

static int *A;
static int *B;
static int *C;

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

    #pragma omp parallel
    {
        #pragma omp for schedule(static)
        for (i = 0; i < n; i++) {
            printf("%3d: T%d\n", i, omp_get_thread_num());
            C[i] = A[i] + B[i];
        }
    }

    for (i = 0; i < n; i++) {
        assert(C[i] == 2*i + 1);
    }

    free(A);
    free(B);
    free(C);

    return 0;
}
