#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

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

int main(int argc, char *argv[])
{
    int n = argc > 1 ? atoi(argv[1]) : 20;

    #pragma omp parallel
    {
        #pragma omp master
        printf("fib(%d) = %d\n", n, fib(n));
    }

    return 0;
}

// RUN: ./fib1 20 | FileCheck %s
//
// CHECK: fib(20) = 6765
