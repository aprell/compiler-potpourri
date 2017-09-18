#ifndef OMP_H
#define OMP_H

void omp_parallel_start(void (*fn)(void *data), void *data, int num_threads);
void omp_parallel_end(void);
void omp_barrier(void);
int omp_get_thread_num(void);
int omp_get_num_threads(void);

#endif // OMP_H
