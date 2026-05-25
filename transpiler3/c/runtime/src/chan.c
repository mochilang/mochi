/*
 * libmochi: bounded ring channel — Phase 9.1.
 *
 * MEP-45 Phase 9.1: implements mochi_chan_make and mochi_chan_free.
 * The typed send/recv helpers are inline in chan.h; only allocation
 * and deallocation live here.
 */
#include "mochi/chan.h"
#include <stdlib.h>
#include <assert.h>

mochi_chan_t *mochi_chan_make(int cap) {
    assert(cap >= 1);
    mochi_chan_t *ch = (mochi_chan_t *)malloc(sizeof(mochi_chan_t));
    if (!ch) abort();
    ch->ring = (void **)malloc((size_t)cap * sizeof(void *));
    if (!ch->ring) abort();
    ch->cap   = cap;
    ch->head  = 0;
    ch->tail  = 0;
    ch->count = 0;
    return ch;
}

void mochi_chan_free(mochi_chan_t *ch) {
    if (!ch) return;
    free(ch->ring);
    free(ch);
}
