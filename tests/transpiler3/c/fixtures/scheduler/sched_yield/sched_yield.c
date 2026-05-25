/*
 * sched_yield: a single fiber yields three times, printing its step
 * number before each yield and after the final resume.
 *
 * Expected output:
 *   step 1
 *   step 2
 *   step 3
 *   done
 */

#include "mochi/sched.h"
#include <stdio.h>

static void counting_fiber(void *unused) {
    (void)unused;
    printf("step 1\n");
    mochi_fiber_yield();
    printf("step 2\n");
    mochi_fiber_yield();
    printf("step 3\n");
    mochi_fiber_yield();
    printf("done\n");
}

void run_scheduler(void) {
    mochi_sched_spawn(counting_fiber, NULL);
    mochi_sched_run();
}
