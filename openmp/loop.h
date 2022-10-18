#ifndef LOOP_H
#define LOOP_H

#include <pthread.h>
#include <stdbool.h>

struct loop {
    pthread_mutex_t lock;
    // [from, to) by step
    int from, to, step;
    int chunk_size;
};

struct loop *loop_init(int from, int to, int step, int chunk_size);
//@requires from < to && step == 1

void loop_destroy(struct loop *loop);

int loop_num_iterations(const struct loop *loop);
//@requires loop != NULL

#define loop_empty(loop) (loop_num_iterations(loop) == 0)

bool loop_split_static(struct loop *loop, int *from, int *to);
//@requires loop != NULL

bool loop_split_dynamic(struct loop *loop, int *from, int *to);
//@requires loop != NULL

bool loop_split_guided(struct loop *loop, int *from, int *to);
//@requires loop != NULL

#endif // LOOP_H
