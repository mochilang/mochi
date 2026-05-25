/*
 * libmochi: cooperative fiber scheduler (Phase 9.0).
 *
 * MEP-45 Phase 9.0 — M:N scheduler foundation using minicoro-compatible
 * stackful fibers. This header exposes the public API consumed by
 * Phase 9.1+ language-level streams and agents.
 *
 * Implementation: transpiler3/c/runtime/src/sched.c (ucontext_t backend).
 *
 * ABI stability: every symbol here is part of libmochi's versioned
 * surface. Existing prototypes never change shape.
 */
#ifndef MOCHI_SCHED_H
#define MOCHI_SCHED_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/* Opaque fiber handle. */
typedef struct mochi_fiber_s mochi_fiber_t;

/* Function signature for fiber entry points. */
typedef void (*mochi_fiber_fn)(void *userdata);

/*
 * mochi_fiber_create -- allocate and initialise a new fiber.
 *
 * fn        : entry point called when the fiber is first resumed.
 * userdata  : arbitrary pointer forwarded to fn; the scheduler does
 *             not inspect it.
 * stack_size: requested stack in bytes; 0 uses MOCHI_DEFAULT_STACK_SIZE
 *             (128 KiB).
 *
 * Returns a pointer to the new fiber. The fiber starts in FIBER_READY
 * state and will not execute until the first mochi_fiber_resume() call.
 * The caller owns the fiber until it reaches FIBER_DEAD state, at
 * which point mochi_fiber_destroy() may be called.
 *
 * Panics (abort) on allocation failure.
 */
mochi_fiber_t *mochi_fiber_create(mochi_fiber_fn fn, void *userdata,
                                  size_t stack_size);

/*
 * mochi_fiber_destroy -- release all resources owned by a dead fiber.
 *
 * The fiber MUST be in FIBER_DEAD state. Destroying a live fiber is
 * undefined behaviour.
 */
void mochi_fiber_destroy(mochi_fiber_t *f);

/*
 * mochi_fiber_resume -- transfer control into a fiber.
 *
 * The caller suspends until the fiber yields (mochi_fiber_yield) or
 * returns from its entry function. On return, the fiber is either in
 * FIBER_SUSPENDED or FIBER_DEAD state.
 *
 * Calling resume on a FIBER_RUNNING or FIBER_DEAD fiber is undefined
 * behaviour.
 */
void mochi_fiber_resume(mochi_fiber_t *f);

/*
 * mochi_fiber_yield -- suspend the currently running fiber.
 *
 * Control returns to the code that called mochi_fiber_resume(). Must
 * only be called from inside a fiber; calling from the main stack is
 * undefined behaviour.
 */
void mochi_fiber_yield(void);

/*
 * mochi_fiber_current -- return the currently executing fiber.
 *
 * Returns NULL when called from the main (non-fiber) stack.
 */
mochi_fiber_t *mochi_fiber_current(void);

/*
 * mochi_sched_spawn -- enqueue a new fiber onto the global scheduler.
 *
 * The fiber is created with the given fn/userdata/stack_size and added
 * to the run queue. mochi_sched_run() drives it to completion.
 * stack_size=0 uses the default.
 */
void mochi_sched_spawn(mochi_fiber_fn fn, void *userdata);

/*
 * mochi_sched_run -- drive the global scheduler until all spawned
 * fibers have completed.
 *
 * Fibers are executed cooperatively in FIFO order. A fiber that yields
 * is placed back at the tail of the queue; a fiber that returns is
 * destroyed and removed from the queue.
 *
 * Returns only when the queue is empty (all fibers dead).
 */
void mochi_sched_run(void);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_SCHED_H */
