#ifndef TASK_H
#define TASK_H

#include <pthread.h>
#include <stdbool.h>

struct task {
    void (*func)(void *);
    void *args;
    struct task *next;
};

struct task_frame {
    unsigned int count;
};

struct task_queue {
    pthread_mutex_t lock;
    struct task *head, *tail;
    unsigned int num_tasks;
};

// Constructor for tasks
#define TASK(func, ...) ({ \
    struct func##_task_args *args = malloc(sizeof(struct func##_task_args)); \
    if (!args) { \
        fprintf(stderr, "Warning: TASK failed\n"); \
        assert(0 && "Runtime error: TASK"); \
    } \
    *args = (struct func##_task_args){ __VA_ARGS__ }; \
    task_init(func##_task_func, args); \
})

struct task *task_init(void (*func)(void *), void *args);
//@requires func != NULL

void task_destroy(struct task *task);

struct task_queue *task_queue_init(void);

void task_queue_destroy(struct task_queue *queue);

void task_enqueue(struct task_queue *queue, struct task *task);
//@requires queue != NULL && task != NULL

struct task *task_dequeue(struct task_queue *queue);
//@requires queue != NULL

void task_run(struct task *task);
//@requires task != NULL

#endif // TASK_H
