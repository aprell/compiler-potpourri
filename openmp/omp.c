#include <assert.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include "omp.h"

struct omp_thread {
    pthread_t handle;
    int ID;
};

struct omp_team {
    struct omp_thread *threads;
    int num_threads;
    pthread_barrier_t barrier;
} team;

void omp_parallel_start(void (*fn)(void *data), void *data, int num_threads)
{
    int i;

    if (num_threads == 0) {
        const char *env = getenv("OMP_NUM_THREADS");
        if (env) {
            num_threads = atoi(env);
            assert(num_threads > 0 && "Invalid value for OMP_NUM_THREADS");
        } else {
            num_threads = sysconf(_SC_NPROCESSORS_ONLN);
        }
    }

    team.threads = malloc(num_threads * sizeof(struct omp_thread));
    assert(team.threads && "Out of memory");
    team.num_threads = num_threads;
    pthread_barrier_init(&team.barrier, NULL, num_threads);

    team.threads[0].handle = pthread_self();
    team.threads[0].ID = 0;

    for (i = 1; i < num_threads; i++) {
        team.threads[i].ID = i;
        pthread_create(&team.threads[i].handle, NULL, (void *(*)(void *))fn, data);
    }
}

void omp_parallel_end(void)
{
    int i;

    for (i = 1; i < team.num_threads; i++) {
        pthread_join(team.threads[i].handle, NULL);
    }

    pthread_barrier_destroy(&team.barrier);
    free(team.threads);
}

void omp_barrier(void)
{
    pthread_barrier_wait(&team.barrier);
}

int omp_get_thread_num(void)
{
    static __thread int ID = -1;
    int i;

    if (ID != -1) return ID;

    for (i = 0; i < team.num_threads; i++) {
        if (pthread_equal(team.threads[i].handle, pthread_self())) {
            ID = team.threads[i].ID;
            return ID;
        }
    }

    assert(0 && "omp_get_thread_num");
}

int omp_get_num_threads(void)
{
    return team.num_threads;
}
