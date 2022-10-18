#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "loop.h"
#include "omp.h"

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

struct loop *loop_init(int from, int to, int step, int chunk_size)
{
    // We assume a canonical form
    assert(from < to && step == 1);

    struct loop *loop = malloc(sizeof(struct loop));
    if (!loop) {
        fprintf(stderr, "Warning: loop_init failed\n");
        return NULL;
    }

    pthread_mutex_init(&loop->lock, NULL);
    loop->from = from;
    loop->to = to;
    loop->step = step;
    loop->chunk_size = chunk_size;

    return loop;
}

void loop_destroy(struct loop *loop)
{
    if (loop) {
        pthread_mutex_destroy(&loop->lock);
        free(loop);
    }
}

int loop_num_iterations(const struct loop *loop)
{
    assert(loop != NULL);

    return (loop->to - loop->from) / loop->step;
}

#define LOCKED(loop) \
    for (int i = (pthread_mutex_lock(&(loop)->lock), 0); !i; pthread_mutex_unlock(&(loop)->lock), i++)

bool loop_split_static(struct loop *loop, int *from, int *to)
{
    assert(loop != NULL);

    static __thread int n = 0;
    int thread_num = omp_get_thread_num();
    int num_threads = omp_get_num_threads();
    int chunk_size, remaining;

    if (loop->chunk_size > 0) {
        // Block-cyclic distribution of iterations
        chunk_size = loop->chunk_size;
        *from = (thread_num + n * num_threads) * chunk_size;
        *to = min(*from + chunk_size, loop->to);

        if (*from < loop->to) {
            n++;
            return true;
        } else {
            // Done
            n = 0;
            return false;
        }
    } else {
        // Block distribution of iterations
        if (n == 0) {
            // First (and only) trip
            chunk_size = loop_num_iterations(loop) / num_threads;
            remaining = loop_num_iterations(loop) % num_threads;
            *from = thread_num * chunk_size + min(thread_num, remaining);
            *to = *from + chunk_size + (thread_num < remaining);
            n = 1;
            return true;
        } else {
            // Done
            n = 0;
            return false;
        }
    }
}

bool loop_split_dynamic(struct loop *loop, int *from, int *to)
{
    assert(loop != NULL);

    bool ret;

    LOCKED(loop) {
        if (!loop_empty(loop)) {
            // Fetch the next iteration
            *from = loop->from++;
            *to = loop->from;
            ret = true;
        } else {
            ret = false;
        }
    }

    return ret;
}

bool loop_split_guided(struct loop *loop, int *from, int *to)
{
    assert(loop != NULL);

    bool ret;

    LOCKED(loop) {
        if (!loop_empty(loop)) {
            // Fetch the next chunk of iterations
            int chunk_size = max(loop_num_iterations(loop) / omp_get_num_threads(), 1);
            *from = loop->from;
            loop->from += chunk_size;
            *to = loop->from;
            ret = true;
        } else {
            ret = false;
        }
    }

    return ret;
}
