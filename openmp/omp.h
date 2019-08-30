#ifndef OMP_H
#define OMP_H

#include <stdbool.h>

void omp_parallel_start(void (*fn)(void *data), void *data, int num_threads);
void omp_parallel_end(void);
void omp_barrier(void);
int omp_get_thread_num(void);
int omp_get_num_threads(void);

bool omp_work_share_init(int from, int to, int step);
bool omp_work_share_destroy(void);

bool omp_split_static(int *from, int *to);
bool omp_split_dynamic(int *from, int *to);
bool omp_split_guided(int *from, int *to);

#endif // OMP_H
