/*
 * libmochi: MPMC broadcast stream — Phase 9.2.
 *
 * MEP-45 Phase 9.2: implements mochi_stream_make, mochi_sub_make,
 * mochi_stream_free, and mochi_sub_free. The typed emit/recv helpers
 * are inline in stream.h; only allocation and deallocation live here.
 */
#include "mochi/stream.h"
#include <stdlib.h>
#include <assert.h>

mochi_stream_t *mochi_stream_make(int cap) {
    assert(cap >= 1);
    mochi_stream_t *s = (mochi_stream_t *)malloc(sizeof(mochi_stream_t));
    if (!s) abort();
    s->ring = (void **)malloc((size_t)cap * sizeof(void *));
    if (!s->ring) abort();
    s->cap   = cap;
    s->tail  = 0;
    s->count = 0;
    return s;
}

mochi_sub_t *mochi_sub_make(mochi_stream_t *s) {
    assert(s != NULL);
    mochi_sub_t *sub = (mochi_sub_t *)malloc(sizeof(mochi_sub_t));
    if (!sub) abort();
    sub->stream = s;
    sub->seen   = s->count; /* start cursor at current emit head */
    return sub;
}

void mochi_stream_free(mochi_stream_t *s) {
    if (!s) return;
    free(s->ring);
    free(s);
}

void mochi_sub_free(mochi_sub_t *sub) {
    free(sub);
}
