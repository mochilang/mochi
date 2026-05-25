/*
 * libmochi: shutdown protocol — Phase 9.4.
 *
 * MEP-45 Phase 9.4: installs SIGINT and SIGTERM handlers that set
 * mochi_shutdown_requested and arm a 5-second SIGALRM hard-kill timer
 * so a stalled fiber drain does not block indefinitely.
 *
 * Windows: all signal machinery is omitted; mochi_shutdown_init is a no-op.
 */
#include "mochi/shutdown.h"

#ifndef _WIN32

#include <signal.h>
#include <unistd.h>

volatile sig_atomic_t mochi_shutdown_requested = 0;

static void shutdown_handler(int sig) {
    (void)sig;
    mochi_shutdown_requested = 1;
    /* Bounded-time hard kill: if the fiber drain stalls, SIGALRM fires
     * after 5 seconds and terminates the process via its default action. */
    alarm(5);
}

void mochi_shutdown_init(void) {
    signal(SIGINT,  shutdown_handler);
    signal(SIGTERM, shutdown_handler);
}

#else /* _WIN32 */

void mochi_shutdown_init(void) { /* no-op on Windows */ }

#endif /* _WIN32 */
