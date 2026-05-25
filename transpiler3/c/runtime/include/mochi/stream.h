/*
 * libmochi: MPMC broadcast stream — Phase 9.2.
 *
 * MEP-45 Phase 9.2: stream<T> is a fixed-capacity ring buffer that
 * broadcasts every emitted value to all subscribers. Each subscriber
 * gets its own cursor into the ring so it can independently consume
 * every element emitted after it subscribed.
 *
 * emit never blocks: if the ring is full the oldest slot is overwritten
 * (lossy for slow subscribers). recv_sub blocks (fiber yields) when
 * the subscriber's cursor has caught up to the emit cursor; calling it
 * outside a fiber context when no new data is available aborts.
 *
 * Typed wrappers for each scalar element type (int, float, bool, string)
 * are inline functions so the C compiler can eliminate the cast overhead.
 *
 * ABI stability: mochi_stream_make, mochi_sub_make, mochi_stream_free,
 * and mochi_sub_free are stable libmochi exports. Inline wrappers are
 * implementation detail.
 */
#ifndef MOCHI_STREAM_H
#define MOCHI_STREAM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "mochi/sched.h"

/* Forward-declare abort without pulling in <stdlib.h>. */
#ifndef MOCHI_CHAN_H
_Noreturn void abort(void);
#endif
/* On WASM32 (4-byte pointer), sizeof(double) > sizeof(void*); float stream
 * slots require heap allocation. Forward-declare malloc/free if chan.h
 * (which already does this) has not been included first. */
#if defined(__wasm__) && !defined(MOCHI_CHAN_H)
extern void *malloc(size_t);
extern void  free(void *);
#endif

/* MPMC broadcast stream ring. */
typedef struct {
    void **ring;   /* circular slot array of capacity cap  */
    int    cap;    /* maximum number of live slots          */
    int    tail;   /* next write index (modulo cap)         */
    int    count;  /* total elements ever emitted (wraps)   */
} mochi_stream_t;

/* Per-subscriber cursor. */
typedef struct {
    mochi_stream_t *stream; /* owning stream                        */
    int             seen;   /* total elements this sub has consumed */
} mochi_sub_t;

/*
 * mochi_stream_make -- allocate a broadcast stream with capacity cap.
 * cap >= 1. Aborts on allocation failure.
 */
mochi_stream_t *mochi_stream_make(int cap);

/*
 * mochi_sub_make -- create a subscriber for stream s.
 * The subscriber's cursor starts at the current emit position, so it
 * will only see values emitted after it subscribed.
 */
mochi_sub_t *mochi_sub_make(mochi_stream_t *s);

/*
 * mochi_stream_free -- release a stream (does not free subscribers).
 */
void mochi_stream_free(mochi_stream_t *s);

/*
 * mochi_sub_free -- release a subscriber handle.
 */
void mochi_sub_free(mochi_sub_t *sub);

/* --- Internal helpers --- */

static inline void mochi__stream_push(mochi_stream_t *s, void *val) {
    s->ring[s->tail] = val;
    s->tail = (s->tail + 1) % s->cap;
    if (s->count - s->count % s->cap < s->cap) {
        /* ring not yet full: safe to just increment */
    }
    s->count++;
}

static inline void *mochi__sub_pop(mochi_sub_t *sub) {
    mochi_stream_t *s = sub->stream;
    while (sub->seen >= s->count) {
        if (mochi_fiber_current() == NULL)
            abort(); /* no new data outside fiber context: programming error */
        mochi_fiber_yield();
    }
    /* The oldest visible slot index: (tail - (count - seen)) mod cap */
    int available = s->count - sub->seen;
    int idx = (s->tail - available + s->cap * 2) % s->cap;
    void *val = s->ring[idx];
    sub->seen++;
    return val;
}

/* --- Typed emit wrappers --- */

static inline void mochi_stream_emit_int(mochi_stream_t *s, int64_t val) {
    mochi__stream_push(s, (void *)(intptr_t)val);
}

static inline void mochi_stream_emit_float(mochi_stream_t *s, double val) {
#if defined(__wasm__)
    double *p = (double *)malloc(sizeof(double));
    if (!p) abort();
    *p = val;
    mochi__stream_push(s, (void *)p);
#else
    void *slot = NULL;
    memcpy(&slot, &val, sizeof(double));
    mochi__stream_push(s, slot);
#endif
}

static inline void mochi_stream_emit_bool(mochi_stream_t *s, int val) {
    mochi__stream_push(s, (void *)(intptr_t)(int64_t)val);
}

static inline void mochi_stream_emit_string(mochi_stream_t *s, const char *val) {
    mochi__stream_push(s, (void *)val);
}

/* --- Typed recv_sub wrappers --- */

static inline int64_t mochi_sub_recv_int(mochi_sub_t *sub) {
    return (int64_t)(intptr_t)mochi__sub_pop(sub);
}

static inline double mochi_sub_recv_float(mochi_sub_t *sub) {
#if defined(__wasm__)
    double *p = (double *)mochi__sub_pop(sub);
    double val = *p;
    free(p);
    return val;
#else
    void *slot = mochi__sub_pop(sub);
    double val;
    memcpy(&val, &slot, sizeof(double));
    return val;
#endif
}

static inline int mochi_sub_recv_bool(mochi_sub_t *sub) {
    return (int)(int64_t)(intptr_t)mochi__sub_pop(sub);
}

static inline const char *mochi_sub_recv_string(mochi_sub_t *sub) {
    return (const char *)mochi__sub_pop(sub);
}

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_STREAM_H */
