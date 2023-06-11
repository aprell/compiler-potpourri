#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

extern int fib(int n);
extern int pow(int b, int e);
extern int fastpow(int b, int e);
extern int sum(int n);
extern int fastsum(int n);
extern int search(int *numbers, int lo, int hi, int x);
extern void sort(int *numbers, int n);

int fib_(int n) {
    return n > 1 ? fib_(n - 1) + fib_(n - 2) : n;
}

void test_fib(void) {
    for (int i = 0; i <= 30; i++) {
        assert(fib(i) == fib_(i));
    }
}

void test_pow(void) {
    for (int i = 0; i <= 10; i++) {
        assert(pow(2, i) == fastpow(2, i));
    }
}

void test_sum(void) {
    for (int i = 0; i <= 100; i++) {
        assert(sum(i) == fastsum(i));
    }
}

void test_search(void) {
    int numbers[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int len = sizeof(numbers) / sizeof(numbers[0]);

    for (int i = 1; i <= 10; i++) {
        assert(search(numbers, 0, len, i) == i - 1);
    }

    assert(search(numbers, 0, len, 0) == -1);
    assert(search(numbers, 0, len, 11) == -1);
    assert(search(numbers, 2, len - 2, 1) == -1);
    assert(search(numbers, 2, len - 2, 9) == -1);
}

void test_sort(void) {
    int numbers[] = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 9};
    int len = sizeof(numbers) / sizeof(numbers[0]);

    sort(numbers, len);

    for (int i = 1; i < len; i++) {
        assert(numbers[i - 1] <= numbers[i]);
    }
}

int main(void) {
    test_fib();
    test_pow();
    test_sum();
    test_search();
    test_sort();

    return 0;
}
