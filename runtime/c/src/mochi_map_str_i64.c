/*
 * MEP-42 Phase 4.2.18 typed-str->i64 map runtime. See
 * mochi_map_str_i64.h.
 */
#include "mochi_map_str_i64.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static uint64_t mochi_map_str_hash(const char *k) {
    /* FNV-1a 64-bit. Fast for short keys and gives a decent
     * distribution after the cap-1 mask; matches the hash family used
     * elsewhere in compiler3's str interning so the runtime is
     * consistent under cc -O2 inlining. */
    uint64_t h = 0xCBF29CE484222325ULL;
    for (const unsigned char *p = (const unsigned char *)k; *p != 0; p++) {
        h ^= (uint64_t)*p;
        h *= 0x100000001B3ULL;
    }
    return h;
}

mochi_map_str_i64 *mochi_map_str_i64_new(void) {
    mochi_map_str_i64 *m = (mochi_map_str_i64 *)malloc(sizeof(mochi_map_str_i64));
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

int64_t mochi_map_str_i64_len(const mochi_map_str_i64 *m) {
    return m->len;
}

static int64_t mochi_map_str_probe(const mochi_map_str_i64 *m, const char *k) {
    /* Returns the bucket index where k lives (if occupied) or the
     * first empty bucket on the probe path (if absent). Caller
     * inspects occ[idx] to disambiguate. Cap must be > 0. */
    uint64_t mask = (uint64_t)m->cap - 1;
    uint64_t i = mochi_map_str_hash(k) & mask;
    while (m->occ[i] != 0 && strcmp(m->keys[i], k) != 0) {
        i = (i + 1) & mask;
    }
    return (int64_t)i;
}

static void mochi_map_str_grow(mochi_map_str_i64 *m) {
    int64_t newcap = m->cap == 0 ? 8 : m->cap * 2;
    const char **nkeys = (const char **)calloc((size_t)newcap, sizeof(const char *));
    int64_t *nvals = (int64_t *)calloc((size_t)newcap, sizeof(int64_t));
    uint8_t *nocc = (uint8_t *)calloc((size_t)newcap, sizeof(uint8_t));
    if (nkeys == NULL || nvals == NULL || nocc == NULL) {
        abort();
    }
    int64_t oldcap = m->cap;
    const char **okeys = m->keys;
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
        const char *k = okeys[b];
        uint64_t i = mochi_map_str_hash(k) & mask;
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

int64_t mochi_map_str_i64_get(const mochi_map_str_i64 *m, const char *k) {
    if (m->cap == 0) {
        return 0;
    }
    int64_t i = mochi_map_str_probe(m, k);
    if (m->occ[i] == 0) {
        return 0;
    }
    return m->vals[i];
}

void mochi_map_str_i64_set(mochi_map_str_i64 *m, const char *k, int64_t v) {
    if (m->cap == 0 || (m->len + 1) * 4 > m->cap * 3) {
        mochi_map_str_grow(m);
    }
    int64_t i = mochi_map_str_probe(m, k);
    if (m->occ[i] == 0) {
        m->occ[i] = 1;
        m->keys[i] = k;
        m->len++;
    }
    m->vals[i] = v;
}

static int mochi_map_str_keycmp(const void *a, const void *b) {
    const char *const *pa = (const char *const *)a;
    const char *const *pb = (const char *const *)b;
    return strcmp(*pa, *pb);
}

mochi_str_array *mochi_map_str_i64_sorted_keys(const mochi_map_str_i64 *m) {
    mochi_str_array *out = mochi_str_array_new();
    if (m->len == 0) {
        return out;
    }
    /* Collect every occupied key into a fresh contiguous buffer, qsort
     * it by strcmp, then push each key into the returned array. The
     * push path doubles cap from 4 on first push and again as needed,
     * which is fine: m->len is the upper bound on capacity here. The
     * intermediate buffer is freed before return; the array's own
     * data backing is owned by the returned mochi_str_array. */
    const char **keys = (const char **)malloc((size_t)m->len * sizeof(const char *));
    if (keys == NULL) {
        abort();
    }
    int64_t n = 0;
    for (int64_t b = 0; b < m->cap; b++) {
        if (m->occ[b] != 0) {
            keys[n++] = m->keys[b];
        }
    }
    qsort(keys, (size_t)n, sizeof(const char *), mochi_map_str_keycmp);
    for (int64_t i = 0; i < n; i++) {
        mochi_str_array_push(out, keys[i]);
    }
    free(keys);
    return out;
}
