/*
 * MEP-42 Phase 4.3.13 wall-clock builtin runtime. See mochi_time.h.
 */
#include "mochi_time.h"

#include <stddef.h>
#include <stdint.h>
#include <sys/time.h>

int64_t mochi_now_us(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (int64_t)tv.tv_sec * 1000000 + (int64_t)tv.tv_usec;
}
