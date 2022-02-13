#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int fib_(int n)
{
    return n > 1 ? fib_(n - 1) + fib_(n - 2) : n;
}

extern int fib(int);

void test_fib(void)
{
    for (int i = 0; i <= 30; i++) {
        assert(fib(i) == fib_(i));
    }
}

extern int pow(int b, int e);
extern int fastpow(int b, int e);

void test_pow(void)
{
    for (int i = 0; i <= 10; i++) {
        assert(pow(2, i) == fastpow(2, i));
    }
}

extern void sort(int *, int);

void test_sort(void)
{
    int numbers[] = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 9};
    int len = sizeof(numbers) / sizeof(numbers[0]);

    sort(numbers, len);

    for (int i = 1; i < len; i++) {
        assert(numbers[i - 1] <= numbers[i]);
    }
}

int main(void)
{
    test_fib();
    test_pow();
    test_sort();

    return 0;
}
