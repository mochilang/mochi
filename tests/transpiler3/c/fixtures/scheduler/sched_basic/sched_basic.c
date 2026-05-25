/*
 * sched_basic: two fibers ping-pong via yield.
 *
 * Expected output (in order):
 *   fiber_a start
 *   fiber_b start
 *   fiber_a end
 *   fiber_b end
 *
 * Execution trace:
 *   main spawns A then B onto the scheduler.
 *   A runs: prints "fiber_a start", yields.
 *   B runs: prints "fiber_b start", yields.
 *   A resumes: prints "fiber_a end", returns.
 *   B resumes: prints "fiber_b end", returns.
 *   Scheduler queue empty; run_scheduler returns.
 */

#include "mochi/sched.h"
#include <stdio.h>

static void fiber_a(void *unused) {
    (void)unused;
    printf("fiber_a start\n");
    mochi_fiber_yield();
    printf("fiber_a end\n");
}

static void fiber_b(void *unused) {
    (void)unused;
    printf("fiber_b start\n");
    mochi_fiber_yield();
    printf("fiber_b end\n");
}

void run_scheduler(void) {
    mochi_sched_spawn(fiber_a, NULL);
    mochi_sched_spawn(fiber_b, NULL);
    mochi_sched_run();
}
