/*
 * MEP-42 Phase 4.3.15.2 typed-i64->i64 map runtime. See
 * mochi_map_i64_i64.h.
 */
#include "mochi_map_i64_i64.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static uint64_t mochi_map_i64_hash(int64_t k) {
    /* SplitMix64 finaliser, used here as a cheap avalanche hash so
     * that consecutive small keys (k_nucleotide uses 0..19) don't
     * pile up on a few buckets after the cap-1 mask. */
    uint64_t x = (uint64_t)k + 0x9E3779B97F4A7C15ULL;
    x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL;
    x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
    x = x ^ (x >> 31);
    return x;
}

mochi_map_i64_i64 *mochi_map_i64_i64_new(void) {
    mochi_map_i64_i64 *m = (mochi_map_i64_i64 *)malloc(sizeof(mochi_map_i64_i64));
    if (m == NULL) {
        abort();
    }
    m->keys = NULL;
    m->vals = NULL;
    m->occ = NULL;
    m->cap = 0;
    m->len = 0;
    return m;
}

static int64_t mochi_map_i64_probe(const mochi_map_i64_i64 *m, int64_t k) {
    /* Returns the bucket index where k lives (if occupied) or the
     * first empty bucket on the probe path (if absent). Caller
     * inspects occ[idx] to disambiguate. Cap must be > 0; callers
     * that might be called on an empty map must grow first. */
    uint64_t mask = (uint64_t)m->cap - 1;
    uint64_t i = mochi_map_i64_hash(k) & mask;
    while (m->occ[i] != 0 && m->keys[i] != k) {
        i = (i + 1) & mask;
    }
    return (int64_t)i;
}

static void mochi_map_i64_grow(mochi_map_i64_i64 *m) {
    int64_t newcap = m->cap == 0 ? 8 : m->cap * 2;
    int64_t *nkeys = (int64_t *)calloc((size_t)newcap, sizeof(int64_t));
    int64_t *nvals = (int64_t *)calloc((size_t)newcap, sizeof(int64_t));
    uint8_t *nocc = (uint8_t *)calloc((size_t)newcap, sizeof(uint8_t));
    if (nkeys == NULL || nvals == NULL || nocc == NULL) {
        abort();
    }
    /* Re-probe every occupied bucket into the new arrays. */
    int64_t oldcap = m->cap;
    int64_t *okeys = m->keys;
    int64_t *ovals = m->vals;
    uint8_t *oocc = m->occ;
    m->keys = nkeys;
    m->vals = nvals;
    m->occ = nocc;
    m->cap = newcap;
    m->len = 0;
    uint64_t mask = (uint64_t)newcap - 1;
    for (int64_t b = 0; b < oldcap; b++) {
        if (oocc[b] == 0) {
            continue;
        }
        int64_t k = okeys[b];
        uint64_t i = mochi_map_i64_hash(k) & mask;
        while (m->occ[i] != 0) {
            i = (i + 1) & mask;
        }
        m->keys[i] = k;
        m->vals[i] = ovals[b];
        m->occ[i] = 1;
        m->len++;
    }
    free(okeys);
    free(ovals);
    free(oocc);
}

int64_t mochi_map_i64_i64_get(const mochi_map_i64_i64 *m, int64_t k) {
    if (m->cap == 0) {
        return 0;
    }
    int64_t i = mochi_map_i64_probe(m, k);
    if (m->occ[i] == 0) {
        return 0;
    }
    return m->vals[i];
}

void mochi_map_i64_i64_set(mochi_map_i64_i64 *m, int64_t k, int64_t v) {
    if (m->cap == 0 || (m->len + 1) * 4 > m->cap * 3) {
        mochi_map_i64_grow(m);
    }
    int64_t i = mochi_map_i64_probe(m, k);
    if (m->occ[i] == 0) {
        m->occ[i] = 1;
        m->keys[i] = k;
        m->len++;
    }
    m->vals[i] = v;
}
