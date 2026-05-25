/*
 * libmochi: bounded ring channel — Phase 9.1.
 *
 * MEP-45 Phase 9.1: chan<T> is a bounded point-to-point ring buffer.
 * send blocks (fiber yields) when full; recv blocks when empty.
 * Both operations are cooperative: they rely on the Phase 9.0 fiber
 * scheduler. Calling send/recv from non-fiber context when the channel
 * is full/empty is a programming error; the runtime aborts.
 *
 * Typed wrappers for each scalar element type (int, float, bool, string)
 * are inline functions so the C compiler can eliminate the cast overhead.
 *
 * ABI stability: mochi_chan_make / mochi_chan_free are stable libmochi
 * exports. The inline wrappers are implementation detail.
 */
#ifndef MOCHI_CHAN_H
#define MOCHI_CHAN_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "mochi/sched.h"

/* Forward-declare abort without pulling in <stdlib.h>, which would conflict
 * with user-defined Mochi functions named abs (a common name). The signature
 * is C11-standard; _Noreturn matches the stdlib attribute on all targets. */
_Noreturn void abort(void);

/* Bounded ring-buffer channel. */
typedef struct {
    void **ring;  /* void* ring buffer of capacity cap              */
    int    cap;   /* maximum number of elements                      */
    int    head;  /* next read position  (modulo cap)                */
    int    tail;  /* next write position (modulo cap)                */
    int    count; /* current number of elements in the buffer        */
} mochi_chan_t;

/*
 * mochi_chan_make -- allocate a new bounded channel with capacity cap.
 * Aborts on allocation failure. cap must be >= 1.
 */
mochi_chan_t *mochi_chan_make(int cap);

/*
 * mochi_chan_free -- release a channel and its ring buffer.
 * Does not drain pending elements first.
 */
void mochi_chan_free(mochi_chan_t *ch);

/* --- Internal helpers --- */

static inline void mochi__chan_push(mochi_chan_t *ch, void *val) {
    while (ch->count >= ch->cap) {
        if (mochi_fiber_current() == NULL)
            abort(); /* full channel with no scheduler: programming error */
        mochi_fiber_yield();
    }
    ch->ring[ch->tail] = val;
    ch->tail = (ch->tail + 1) % ch->cap;
    ch->count++;
}

static inline void *mochi__chan_pop(mochi_chan_t *ch) {
    while (ch->count == 0) {
        if (mochi_fiber_current() == NULL)
            abort(); /* empty channel with no scheduler: programming error */
        mochi_fiber_yield();
    }
    void *val = ch->ring[ch->head];
    ch->head = (ch->head + 1) % ch->cap;
    ch->count--;
    return val;
}

/* --- Typed send/recv wrappers --- */

/* int (int64_t stored as pointer-sized integer via intptr_t cast) */
static inline void mochi_chan_send_int(mochi_chan_t *ch, int64_t val) {
    mochi__chan_push(ch, (void *)(intptr_t)val);
}
static inline int64_t mochi_chan_recv_int(mochi_chan_t *ch) {
    return (int64_t)(intptr_t)mochi__chan_pop(ch);
}

/* float (double stored by copying bits into a pointer-sized slot) */
static inline void mochi_chan_send_float(mochi_chan_t *ch, double val) {
    void *slot = NULL;
    memcpy(&slot, &val, sizeof(double));
    mochi__chan_push(ch, slot);
}
static inline double mochi_chan_recv_float(mochi_chan_t *ch) {
    void *slot = mochi__chan_pop(ch);
    double val;
    memcpy(&val, &slot, sizeof(double));
    return val;
}

/* bool (stored as int64_t 0 or 1, same as mochi runtime convention) */
static inline void mochi_chan_send_bool(mochi_chan_t *ch, int val) {
    mochi__chan_push(ch, (void *)(intptr_t)(int64_t)val);
}
static inline int mochi_chan_recv_bool(mochi_chan_t *ch) {
    return (int)(int64_t)(intptr_t)mochi__chan_pop(ch);
}

/* string (const char * stored directly as a pointer) */
static inline void mochi_chan_send_string(mochi_chan_t *ch, const char *val) {
    mochi__chan_push(ch, (void *)val);
}
static inline const char *mochi_chan_recv_string(mochi_chan_t *ch) {
    return (const char *)mochi__chan_pop(ch);
}

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_CHAN_H */
