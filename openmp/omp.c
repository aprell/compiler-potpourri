#include <assert.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include "loop.h"
#include "omp.h"

struct omp_thread {
    struct {
        void (*fn)(void *);
        void *data;
    } /* implicit */ task;
    pthread_t handle;
    int ID;
};

struct omp_team {
    pthread_mutex_t lock;
    struct omp_thread *threads;
    struct loop *work_share;
    struct task_queue *tasks;
    pthread_barrier_t barrier;
    int num_threads;
} team;

static __thread struct omp_thread *self;

static void *thread_entry_fn(void *args)
{
    self = (struct omp_thread *)args;
    assert(self->handle == pthread_self());

    self->task.fn(self->task.data);

    return NULL;
}

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
    pthread_mutex_init(&team.lock, NULL);
    team.work_share = NULL;
    team.tasks = task_queue_init();
    pthread_barrier_init(&team.barrier, NULL, num_threads);
    team.num_threads = num_threads;

    team.threads[0].task.fn = fn;
    team.threads[0].task.data = data;
    team.threads[0].handle = pthread_self();
    team.threads[0].ID = 0;
    self = &team.threads[0];

    for (i = 1; i < num_threads; i++) {
        team.threads[i].task.fn = fn;
        team.threads[i].task.data = data;
        team.threads[i].ID = i;
        pthread_create(&team.threads[i].handle, NULL, thread_entry_fn, &team.threads[i]);
    }
}

void omp_parallel_end(void)
{
    int i;

    for (i = 1; i < team.num_threads; i++) {
        pthread_join(team.threads[i].handle, NULL);
    }

    pthread_barrier_destroy(&team.barrier);
    omp_work_share_destroy();
    task_queue_destroy(team.tasks);
    free(team.threads);
}

void omp_barrier(void)
{
    pthread_barrier_wait(&team.barrier);
}

int omp_get_thread_num(void)
{
    return self->ID;
}

int omp_get_num_threads(void)
{
    return team.num_threads;
}

#define LOCKED(team) \
    for (int i = (pthread_mutex_lock(&(team)->lock), 0); !i; pthread_mutex_unlock(&(team)->lock), i++)

bool omp_work_share_init(int from, int to, int step)
{
    bool ret;

    LOCKED(&team) {
        if (!team.work_share) {
            team.work_share = loop_init(from, to, step);
            ret = true;
        } else {
            ret = false;
        }
    }

    return ret;
}

bool omp_work_share_destroy(void)
{
    bool ret;

    LOCKED(&team) {
        if (team.work_share) {
            loop_destroy(team.work_share);
            team.work_share = NULL;
            ret = true;
        } else {
            ret = false;
        }
    }

    return ret;
}

bool omp_split_static(int *from, int *to)
{
    assert(team.work_share != NULL);

    return loop_split_static(team.work_share, from, to);
}

bool omp_split_dynamic(int *from, int *to)
{
    assert(team.work_share != NULL);

    return loop_split_dynamic(team.work_share, from, to);
}

bool omp_split_guided(int *from, int *to)
{
    assert(team.work_share != NULL);

    return loop_split_guided(team.work_share, from, to);
}

void omp_task_enqueue_(struct task *task)
{
    assert(team.tasks != NULL);

    task_enqueue(team.tasks, task);
}

#define READ_ONCE(x) (*(volatile typeof(x) *)&(x))

void omp_taskwait(struct task_frame *frame)
{
    assert(frame != NULL);

    while (READ_ONCE(frame->count) != 0) {
        struct task *task = task_dequeue(team.tasks);
        if (task) task_run(task);
    }
}
