/*
 * MEP-42 Phase 4.3.13 wall-clock builtin runtime.
 *
 * Backs Mochi's `now()` builtin when the program is compiled with
 * `mochi build --target=c`. The IR op OpNow lowers to a call into
 * mochi_now_us().
 *
 * mochi_now_us returns the current wall-clock time in microseconds
 * since the Unix epoch as int64_t. The unit and reference epoch match
 * the Go target's `time.Now().UnixMicro()` so cross-target outputs
 * that take deltas of two calls agree on the magnitude.
 *
 * Implementation note: POSIX gettimeofday is used directly. clock_gettime
 * (CLOCK_REALTIME) would give nanosecond precision but Mochi's `now()`
 * contract is microsecond, so the cheaper call is sufficient.
 */
#ifndef MOCHI_RUNTIME_C_TIME_H
#define MOCHI_RUNTIME_C_TIME_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

int64_t mochi_now_us(void);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_TIME_H */
