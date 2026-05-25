/*
 * libmochi: shutdown protocol (Phase 9.4).
 *
 * MEP-45 Phase 9.4 — graceful SIGINT/SIGTERM drain with bounded-time
 * hard kill. mochi_shutdown_init() is called at the start of every
 * generated main() function. The scheduler (sched.c) checks
 * mochi_shutdown_requested after each fiber yield and drains the queue
 * when it is set.
 *
 * Platform notes:
 *   - POSIX signals (SIGINT, SIGTERM, alarm) are not available on Windows.
 *     mochi_shutdown_init() is a no-op on _WIN32; the flag is not declared.
 */
#ifndef MOCHI_SHUTDOWN_H
#define MOCHI_SHUTDOWN_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32
#include <signal.h>
/*
 * Set to 1 by the SIGINT/SIGTERM handler. The scheduler run loop and
 * any blocking chan/stream pop checks this flag to exit early on shutdown.
 */
extern volatile sig_atomic_t mochi_shutdown_requested;
#endif /* _WIN32 */

/*
 * mochi_shutdown_init -- install SIGINT and SIGTERM handlers.
 *
 * Called once at the top of the generated main(). On shutdown, sets
 * mochi_shutdown_requested = 1 and arms a 5-second alarm so a stalled
 * program is hard-killed by SIGALRM if it does not drain in time.
 * No-op on Windows (_WIN32).
 */
void mochi_shutdown_init(void);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_SHUTDOWN_H */
