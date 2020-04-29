#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "task.h"

struct task *task_init(void (*func)(void *), void *args)
{
    assert(func != NULL);

    struct task *task = malloc(sizeof(struct task));
    if (!task) {
        fprintf(stderr, "Warning: task_alloc failed\n");
        return NULL;
    }

    struct task_frame *frame = *(struct task_frame **)args;
    if (frame) __sync_add_and_fetch(&frame->count, 1);

    task->func = func;
    task->args = args;
    task->next = NULL;

    return task;
}

void task_destroy(struct task *task)
{
    if (task) {
        free(task->args);
        free(task);
    }
}

struct task_queue *task_queue_init(void)
{
    struct task_queue *queue = malloc(sizeof(struct task_queue));
    if (!queue) {
        fprintf(stderr, "Warning: task_queue_init failed\n");
        return NULL;
    }

    pthread_mutex_init(&queue->lock, NULL);
    queue->head = NULL;
    queue->tail = NULL;
    queue->num_tasks = 0;

    return queue;
}

static bool task_queue_empty(struct task_queue *queue)
{
    assert(queue != NULL);

    if (queue->head == NULL) {
        assert(queue->tail == NULL);
        assert(queue->num_tasks == 0);
        return true;
    } else {
        assert(queue->tail != NULL);
        assert(queue->num_tasks > 0);
        return false;
    }
}

void task_queue_destroy(struct task_queue *queue)
{
    if (!queue) return;

    struct task *task;

    while ((task = task_dequeue(queue)) != NULL) {
        task_destroy(task);
    }

    assert(task_queue_empty(queue));
    pthread_mutex_destroy(&queue->lock);
    free(queue);
}

#define LOCKED(queue) \
    for (int i = (pthread_mutex_lock(&(queue)->lock), 0); !i; pthread_mutex_unlock(&(queue)->lock), i++)

void task_enqueue(struct task_queue *queue, struct task *task)
{
    assert(queue != NULL);
    assert(task != NULL);

    LOCKED(queue) {
        if (task_queue_empty(queue)) {
            queue->head = queue->tail = task;
        } else {
            queue->tail->next = task;
            queue->tail = task;
        }
        queue->num_tasks++;
    }
}

struct task *task_dequeue(struct task_queue *queue)
{
    assert(queue != NULL);

    struct task *task;

    LOCKED(queue) {
        task = queue->head;
        if (task == NULL) {
            assert(task_queue_empty(queue));
            // Unlock queue and leave the block
            continue;
        }
        queue->head = queue->head->next;
        if (queue->head == NULL) {
            assert(queue->tail == task);
            queue->tail = NULL;
        }
        task->next = NULL;
        queue->num_tasks--;
    }

    return task;
}

void task_run(struct task *task)
{
    assert(task != NULL);

    task->func(task->args);

    struct task_frame *frame = *(struct task_frame **)task->args;
    if (frame) __sync_sub_and_fetch(&frame->count, 1);

    task_destroy(task);
}
