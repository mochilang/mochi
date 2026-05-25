/*
 * shutdown_sched: two fibers run normally to completion with the
 * shutdown handler installed. Verifies that mochi_shutdown_requested == 0
 * (no signal received) does not affect normal scheduler behavior.
 */
#include "mochi/sched.h"
#include "mochi/shutdown.h"
#include <stdio.h>

static void fiber_x(void *unused) {
    (void)unused;
    printf("fiber_x\n");
    mochi_fiber_yield();
    printf("fiber_x done\n");
}

static void fiber_y(void *unused) {
    (void)unused;
    printf("fiber_y\n");
    mochi_fiber_yield();
    printf("fiber_y done\n");
}

void run_scheduler(void) {
    mochi_sched_spawn(fiber_x, NULL);
    mochi_sched_spawn(fiber_y, NULL);
    mochi_sched_run();
}
