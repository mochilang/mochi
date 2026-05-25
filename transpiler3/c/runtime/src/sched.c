/*
 * libmochi: cooperative fiber scheduler — Phase 9.0.
 *
 * MEP-45 Phase 9.0: lightweight M:N cooperative scheduler built on
 * POSIX ucontext_t. Provides the same external API surface as the
 * planned minicoro substrate (MEP-45 §10), so Phase 9.1+ can swap in
 * the real minicoro header without touching call sites.
 *
 * Platform notes:
 *   - ucontext_t is POSIX.1-2001 and ships on Linux and macOS.
 *   - macOS's ucontext.h guards itself with "#error ... requires
 *     _XOPEN_SOURCE to be defined".  We define _XOPEN_SOURCE 600
 *     (SUSv3/POSIX.1-2001) before any system header so the guard
 *     passes.  The definition must precede all #includes.
 *   - _XOPEN_SOURCE additionally silences the Clang deprecation
 *     warnings on Apple platforms, so no extra pragma is needed.
 *   - The two-uint32 argument trick used in fiber_entry is the
 *     canonical portable way to pass a 64-bit pointer through the
 *     int-sized makecontext variadic arguments on both ILP32 and LP64
 *     ABIs (POSIX explicitly endorses this pattern).
 *   - __wasm__ (Phase 12.2): ucontext is unavailable under wasm32-wasi;
 *     a single-fibre synchronous stub is compiled instead.
 */

#ifndef __wasm__

/* Must be defined before any system header on macOS to unlock ucontext. */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

/*
 * On Apple platforms the ucontext API is tagged deprecated (first
 * deprecated macOS 10.6). We suppress the warning so that -Wall does
 * not produce noise: the API still works and is the only portable
 * stackful-coroutine primitive available without an external library.
 */
#ifdef __APPLE__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "mochi/sched.h"
#include "mochi/shutdown.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ucontext.h>

/* --------------------------------------------------------------------- */
/* Internal constants                                                      */
/* --------------------------------------------------------------------- */

#define MOCHI_DEFAULT_STACK_SIZE (128u * 1024u)

/*
 * Maximum number of fibers the global scheduler queue can hold at once.
 * The queue is a fixed-size circular buffer; spawning more than this
 * many concurrently-live fibers is a programming error. Phase 9.3 will
 * replace this with a growable deque.
 */
#define MOCHI_SCHED_MAX_FIBERS 256

/* --------------------------------------------------------------------- */
/* Fiber state machine                                                     */
/* --------------------------------------------------------------------- */

typedef enum {
    FIBER_READY,      /* created, not yet started */
    FIBER_RUNNING,    /* currently executing on the CPU */
    FIBER_SUSPENDED,  /* yielded; waiting to be resumed */
    FIBER_DEAD        /* entry function has returned */
} fiber_state;

/* --------------------------------------------------------------------- */
/* Fiber struct                                                            */
/* --------------------------------------------------------------------- */

struct mochi_fiber_s {
    ucontext_t ctx;         /* fiber execution context */
    ucontext_t caller_ctx;  /* saved caller context (resume site) */
    char      *stack;       /* heap-allocated stack buffer */
    size_t     stack_size;  /* size of stack buffer in bytes */
    mochi_fiber_fn fn;      /* user entry point */
    void      *userdata;    /* forwarded to fn */
    fiber_state state;
};

/* --------------------------------------------------------------------- */
/* Thread-local current fiber                                              */
/* --------------------------------------------------------------------- */

/*
 * Points to the fiber currently executing on this thread, or NULL when
 * running on the main (non-fiber) stack. Phase 9.3 will move this to a
 * true thread-local once multi-threaded OS threads are introduced.
 */
static mochi_fiber_t *__mochi_current_fiber = NULL;

/* --------------------------------------------------------------------- */
/* Global scheduler queue (simple circular buffer, FIFO)                  */
/* --------------------------------------------------------------------- */

static mochi_fiber_t *__sched_queue[MOCHI_SCHED_MAX_FIBERS];
static int __sched_head = 0; /* index of next item to dequeue */
static int __sched_tail = 0; /* index where next item will be enqueued */

static int sched_queue_size(void) {
    return (__sched_tail - __sched_head + MOCHI_SCHED_MAX_FIBERS)
           % MOCHI_SCHED_MAX_FIBERS;
}

static void sched_enqueue(mochi_fiber_t *f) {
    __sched_queue[__sched_tail] = f;
    __sched_tail = (__sched_tail + 1) % MOCHI_SCHED_MAX_FIBERS;
}

static mochi_fiber_t *sched_dequeue(void) {
    mochi_fiber_t *f = __sched_queue[__sched_head];
    __sched_head = (__sched_head + 1) % MOCHI_SCHED_MAX_FIBERS;
    return f;
}

/* --------------------------------------------------------------------- */
/* Fiber entry trampoline                                                  */
/* --------------------------------------------------------------------- */

/*
 * fiber_entry is the function passed to makecontext(). POSIX requires
 * that makecontext arguments be int-sized, so a 64-bit pointer is split
 * into two uint32_t halves and reassembled here.
 */
static void fiber_entry(uint32_t hi, uint32_t lo) {
    mochi_fiber_t *f =
        (mochi_fiber_t *)(((uintptr_t)hi << 32) | (uintptr_t)lo);

    f->fn(f->userdata);

    /* Mark the fiber dead and return control to the resume caller. */
    f->state = FIBER_DEAD;
    swapcontext(&f->ctx, &f->caller_ctx);

    /* Unreachable: swapcontext does not return here. */
    abort();
}

/* --------------------------------------------------------------------- */
/* Public API: fiber lifecycle                                             */
/* --------------------------------------------------------------------- */

mochi_fiber_t *mochi_fiber_create(mochi_fiber_fn fn, void *userdata,
                                  size_t stack_size) {
    if (stack_size == 0) {
        stack_size = MOCHI_DEFAULT_STACK_SIZE;
    }

    mochi_fiber_t *f = (mochi_fiber_t *)malloc(sizeof(mochi_fiber_t));
    if (!f) abort();
    memset(f, 0, sizeof(*f));

    f->stack = (char *)malloc(stack_size);
    if (!f->stack) abort();

    f->stack_size = stack_size;
    f->fn         = fn;
    f->userdata   = userdata;
    f->state      = FIBER_READY;

    /* Initialise the ucontext for the fiber. */
    if (getcontext(&f->ctx) != 0) abort();
    f->ctx.uc_stack.ss_sp   = f->stack;
    f->ctx.uc_stack.ss_size = stack_size;
    f->ctx.uc_link          = NULL; /* we manage the link via swapcontext */

    /*
     * Split the pointer into two 32-bit halves for makecontext.
     * This pattern is sanctioned by POSIX (see Austin Group Defect 63).
     */
    uintptr_t ptr = (uintptr_t)f;
    uint32_t  hi  = (uint32_t)(ptr >> 32);
    uint32_t  lo  = (uint32_t)(ptr & 0xFFFFFFFFu);
    makecontext(&f->ctx, (void (*)(void))fiber_entry, 2, hi, lo);

    return f;
}

void mochi_fiber_destroy(mochi_fiber_t *f) {
    if (!f) return;
    free(f->stack);
    free(f);
}

/* --------------------------------------------------------------------- */
/* Public API: execution                                                   */
/* --------------------------------------------------------------------- */

void mochi_fiber_resume(mochi_fiber_t *f) {
    mochi_fiber_t *prev    = __mochi_current_fiber;
    __mochi_current_fiber  = f;
    f->state               = FIBER_RUNNING;

    /*
     * swapcontext saves the current register state into f->caller_ctx
     * and restores f->ctx, transferring control to the fiber.
     * When the fiber yields or returns, swapcontext in fiber_entry or
     * mochi_fiber_yield restores caller_ctx here.
     */
    swapcontext(&f->caller_ctx, &f->ctx);

    __mochi_current_fiber = prev;
}

void mochi_fiber_yield(void) {
    mochi_fiber_t *f = __mochi_current_fiber;
    f->state = FIBER_SUSPENDED;
    swapcontext(&f->ctx, &f->caller_ctx);
    /* Execution resumes here after the next mochi_fiber_resume(). */
    f->state = FIBER_RUNNING;
}

mochi_fiber_t *mochi_fiber_current(void) {
    return __mochi_current_fiber;
}

/* --------------------------------------------------------------------- */
/* Public API: global scheduler                                            */
/* --------------------------------------------------------------------- */

void mochi_sched_spawn(mochi_fiber_fn fn, void *userdata) {
    mochi_fiber_t *f = mochi_fiber_create(fn, userdata, 0);
    sched_enqueue(f);
}

void mochi_sched_run(void) {
    while (sched_queue_size() > 0) {
#ifndef _WIN32
        /* Phase 9.4: abort the run loop when shutdown is requested so
         * the program can exit gracefully after the current fiber round. */
        if (mochi_shutdown_requested) break;
#endif
        mochi_fiber_t *f = sched_dequeue();

        /* Skip any fibers that somehow ended up dead in the queue. */
        if (f->state == FIBER_DEAD) {
            mochi_fiber_destroy(f);
            continue;
        }

        mochi_fiber_resume(f);

        if (f->state == FIBER_DEAD) {
            mochi_fiber_destroy(f);
        } else {
            /* Fiber yielded; put it back at the tail of the queue. */
            sched_enqueue(f);
        }
    }
}

#ifdef __APPLE__
#pragma clang diagnostic pop
#endif

#else /* __wasm__: single-fibre synchronous execution stub (Phase 12.2) */

/*
 * WASM/WASI does not provide ucontext_t or any stackful-coroutine primitive.
 * This stub maps the scheduler API onto a simple synchronous call model:
 *   - mochi_fiber_resume calls fn(userdata) immediately and blocks until it returns.
 *   - mochi_fiber_yield is a no-op (fibers run to completion without preemption).
 *   - mochi_fiber_current always returns NULL (no active fiber context).
 *   - mochi_sched_run drains the spawn queue by calling each fn in FIFO order.
 *
 * This is correct for the Phase 12.2 narrowed surface: chan<T>/stream<T>/agent
 * operations that would block (empty recv, full send) abort rather than yield,
 * which is the right behaviour for a single-threaded WASM environment where
 * there is no other fiber to unblock the channel.
 */

#include "mochi/sched.h"
#include <stdlib.h>

#define MOCHI_WASM_MAX_FIBERS 256

typedef enum {
    FIBER_READY,
    FIBER_RUNNING,
    FIBER_DEAD
} wasm_fiber_state;

struct mochi_fiber_s {
    mochi_fiber_fn   fn;
    void            *userdata;
    wasm_fiber_state state;
};

static mochi_fiber_t *__wasm_queue[MOCHI_WASM_MAX_FIBERS];
static int __wasm_head = 0;
static int __wasm_tail = 0;

static int wasm_queue_size(void) {
    return (__wasm_tail - __wasm_head + MOCHI_WASM_MAX_FIBERS)
           % MOCHI_WASM_MAX_FIBERS;
}

static void wasm_enqueue(mochi_fiber_t *f) {
    __wasm_queue[__wasm_tail] = f;
    __wasm_tail = (__wasm_tail + 1) % MOCHI_WASM_MAX_FIBERS;
}

static mochi_fiber_t *wasm_dequeue(void) {
    mochi_fiber_t *f = __wasm_queue[__wasm_head];
    __wasm_head = (__wasm_head + 1) % MOCHI_WASM_MAX_FIBERS;
    return f;
}

mochi_fiber_t *mochi_fiber_create(mochi_fiber_fn fn, void *userdata,
                                  size_t stack_size) {
    (void)stack_size;
    mochi_fiber_t *f = (mochi_fiber_t *)malloc(sizeof(mochi_fiber_t));
    if (!f) abort();
    f->fn       = fn;
    f->userdata = userdata;
    f->state    = FIBER_READY;
    return f;
}

void mochi_fiber_destroy(mochi_fiber_t *f) {
    free(f);
}

void mochi_fiber_resume(mochi_fiber_t *f) {
    f->state = FIBER_RUNNING;
    f->fn(f->userdata);
    f->state = FIBER_DEAD;
}

/* No-op: single synchronous fibre; yield has no peer to schedule. */
void mochi_fiber_yield(void) {}

/* Always NULL: no active fibre context under the synchronous stub. */
mochi_fiber_t *mochi_fiber_current(void) { return NULL; }

void mochi_sched_spawn(mochi_fiber_fn fn, void *userdata) {
    mochi_fiber_t *f = mochi_fiber_create(fn, userdata, 0);
    wasm_enqueue(f);
}

void mochi_sched_run(void) {
    while (wasm_queue_size() > 0) {
        mochi_fiber_t *f = wasm_dequeue();
        if (f->state == FIBER_DEAD) {
            mochi_fiber_destroy(f);
            continue;
        }
        mochi_fiber_resume(f);
        if (f->state == FIBER_DEAD) {
            mochi_fiber_destroy(f);
        } else {
            wasm_enqueue(f);
        }
    }
}

#endif /* __wasm__ */
