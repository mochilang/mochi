/*
 * sched_nested: three fibers (producer, consumer, observer) interleave
 * cooperatively. Each fiber yields once between its two print statements,
 * so the round-robin scheduler interleaves them.
 *
 * Spawn order: producer, consumer, observer.
 * Expected output (round-robin over three fibers, each yielding once):
 *   producer: send
 *   consumer: recv
 *   observer: watch
 *   producer: done
 *   consumer: done
 *   observer: done
 */

#include "mochi/sched.h"
#include <stdio.h>

static void producer(void *unused) {
    (void)unused;
    printf("producer: send\n");
    mochi_fiber_yield();
    printf("producer: done\n");
}

static void consumer(void *unused) {
    (void)unused;
    printf("consumer: recv\n");
    mochi_fiber_yield();
    printf("consumer: done\n");
}

static void observer(void *unused) {
    (void)unused;
    printf("observer: watch\n");
    mochi_fiber_yield();
    printf("observer: done\n");
}

void run_scheduler(void) {
    mochi_sched_spawn(producer, NULL);
    mochi_sched_spawn(consumer, NULL);
    mochi_sched_spawn(observer, NULL);
    mochi_sched_run();
}
