/*
 * libmochi: map runtime implementation.
 *
 * MEP-45 Phase 3.2. See website/docs/mep/mep-0045.md and
 * website/docs/implementation/0045/phase-03-records-collections.md
 * sub-phase 3.2.
 *
 * Identity. Portable C; only <stdint.h>, <stdlib.h>, <string.h>
 * required. Eight (K,V) instantiations share the open-addressing
 * + linear-probe + load-factor-0.5 algorithm; the per-(K,V) code
 * is expanded once via the MOCHI_MAP_DEFINE macro below.
 *
 * Macro choice. Phase 3.4 ships a real monomorphisation pass that
 * generates this code from an aotir walk; until then a single
 * macro is the most compact way to keep eight instantiations
 * consistent. Each helper compiles into its own externally-visible
 * symbol so the linker deduplicates by name (not by macro identity)
 * and the resulting libmochi looks identical to hand-written code
 * under nm.
 *
 * Hash. SplitMix64 finalizer for i64 keys (mirrors vm3 maps.go so
 * fixtures that count hash-table collisions stay byte-equal across
 * the oracle and the AOT path). FNV-1a-64 for string keys (the
 * cheapest portable hash with a uniform distribution that doesn't
 * pull in any extra deps). Live hashes are forced odd (`| 1`) so
 * the zero-value of an entry slot unambiguously means "empty",
 * matching the vm3 convention.
 *
 * Sort. Insertion sort over a fresh snapshot of the live entries.
 * O(n^2) suffices for fixture-scale maps (worst-case n ~ 100); the
 * deterministic-by-construction style matches Phase 3.1's list
 * helpers. Heap-sort or merge-sort drops in for Phase 18 perf.
 */
#include "mochi/map.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "mochi/errors.h"
#include "mochi/list.h"

#define MOCHI_MAP_INIT_CAP 8

/* SplitMix64 finalizer; matches runtime/vm3/maps.go::hashI64. */
static inline uint64_t mochi_hash_i64(int64_t k) {
    uint64_t x = (uint64_t)k;
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebULL;
    x ^= x >> 31;
    return x | 1ULL;
}

/* FNV-1a-64 over the NUL-terminated key. */
static inline uint64_t mochi_hash_str(const char *s) {
    uint64_t h = 0xcbf29ce484222325ULL;
    while (*s) {
        h ^= (uint64_t)(unsigned char)*s++;
        h *= 0x100000001b3ULL;
    }
    return h | 1ULL;
}

#define MOCHI_EQ_I64(a, b) ((a) == (b))
#define MOCHI_EQ_STR(a, b) (strcmp((a), (b)) == 0)

#define MOCHI_LT_I64(a, b) ((a) < (b))
#define MOCHI_LT_STR(a, b) (strcmp((a), (b)) < 0)

/*
 * MOCHI_MAP_DEFINE expands into a full per-(K,V) helper set:
 *   _lit / _get / _has / _len / _snapshot / _keys / _values.
 *
 * KT and VT are the storage element types (e.g., int64_t, double,
 * const char *). KARR and VARR are the corresponding "array of T"
 * parameter spellings used by _lit; for pointer-typed T they drop
 * the redundant inner const (e.g., for KT = const char *, KARR is
 * `const char **` rather than `const const char * *`, which would
 * trigger -Wduplicate-decl-specifier).
 */
#define MOCHI_MAP_DEFINE(NAME, KT, VT, KARR, VARR, HASH, EQ, LT, LIST_K, LIST_V) \
                                                                               \
NAME NAME##_lit(KARR keys, VARR values, int64_t n) {                           \
    NAME out;                                                                  \
    int64_t cap = MOCHI_MAP_INIT_CAP;                                          \
    while (cap < 2 * (n + 1)) cap <<= 1;                                       \
    out.table = (NAME##_entry *)calloc((size_t)cap, sizeof(NAME##_entry));     \
    out.cap = cap;                                                             \
    out.nLive = 0;                                                             \
    if (out.table == NULL) mochi_panic_index();                                \
    for (int64_t i = 0; i < n; i++) {                                          \
        uint64_t h = HASH(keys[i]);                                            \
        uint64_t mask = (uint64_t)(cap - 1);                                   \
        uint64_t pos = h & mask;                                               \
        while (1) {                                                            \
            NAME##_entry *e = &out.table[pos];                                 \
            if (e->hash == 0) {                                                \
                e->hash = h;                                                   \
                e->key = keys[i];                                              \
                e->value = values[i];                                          \
                out.nLive++;                                                   \
                break;                                                         \
            }                                                                  \
            if (e->hash == h && EQ(e->key, keys[i])) {                         \
                e->value = values[i];                                          \
                break;                                                         \
            }                                                                  \
            pos = (pos + 1) & mask;                                            \
        }                                                                      \
    }                                                                          \
    return out;                                                                \
}                                                                              \
                                                                               \
VT NAME##_get(NAME m, KT k) {                                                  \
    if (m.cap == 0) mochi_panic_index();                                       \
    uint64_t h = HASH(k);                                                      \
    uint64_t mask = (uint64_t)(m.cap - 1);                                     \
    uint64_t pos = h & mask;                                                   \
    while (1) {                                                                \
        NAME##_entry *e = &m.table[pos];                                       \
        if (e->hash == 0) mochi_panic_index();                                 \
        if (e->hash == h && EQ(e->key, k)) return e->value;                    \
        pos = (pos + 1) & mask;                                                \
    }                                                                          \
}                                                                              \
                                                                               \
int NAME##_has(NAME m, KT k) {                                                 \
    if (m.cap == 0) return 0;                                                  \
    uint64_t h = HASH(k);                                                      \
    uint64_t mask = (uint64_t)(m.cap - 1);                                     \
    uint64_t pos = h & mask;                                                   \
    while (1) {                                                                \
        NAME##_entry *e = &m.table[pos];                                       \
        if (e->hash == 0) return 0;                                            \
        if (e->hash == h && EQ(e->key, k)) return 1;                           \
        pos = (pos + 1) & mask;                                                \
    }                                                                          \
}                                                                              \
                                                                               \
int64_t NAME##_len(NAME m) { return m.nLive; }                                 \
                                                                               \
static void NAME##_snapshot(NAME m, KT *out_keys, VT *out_vals) {              \
    int64_t k = 0;                                                             \
    for (int64_t i = 0; i < m.cap; i++) {                                      \
        NAME##_entry *e = &m.table[i];                                         \
        if (e->hash == 0) continue;                                            \
        out_keys[k] = e->key;                                                  \
        out_vals[k] = e->value;                                                \
        k++;                                                                   \
    }                                                                          \
    for (int64_t i = 1; i < m.nLive; i++) {                                    \
        KT key = out_keys[i];                                                  \
        VT val = out_vals[i];                                                  \
        int64_t j = i - 1;                                                     \
        while (j >= 0 && LT(key, out_keys[j])) {                               \
            out_keys[j + 1] = out_keys[j];                                     \
            out_vals[j + 1] = out_vals[j];                                     \
            j--;                                                               \
        }                                                                      \
        out_keys[j + 1] = key;                                                 \
        out_vals[j + 1] = val;                                                 \
    }                                                                          \
}                                                                              \
                                                                               \
LIST_K NAME##_keys(NAME m) {                                                   \
    size_t kbytes = (size_t)(m.nLive > 0 ? m.nLive : 1) * sizeof(KT);          \
    size_t vbytes = (size_t)(m.nLive > 0 ? m.nLive : 1) * sizeof(VT);          \
    KT *ks = (KT *)malloc(kbytes);                                             \
    VT *vs = (VT *)malloc(vbytes);                                             \
    if (ks == NULL || vs == NULL) mochi_panic_index();                         \
    NAME##_snapshot(m, ks, vs);                                                \
    LIST_K out = LIST_K##_lit(ks, m.nLive);                                    \
    free(ks);                                                                  \
    free(vs);                                                                  \
    return out;                                                                \
}                                                                              \
                                                                               \
LIST_V NAME##_values(NAME m) {                                                 \
    size_t kbytes = (size_t)(m.nLive > 0 ? m.nLive : 1) * sizeof(KT);          \
    size_t vbytes = (size_t)(m.nLive > 0 ? m.nLive : 1) * sizeof(VT);          \
    KT *ks = (KT *)malloc(kbytes);                                             \
    VT *vs = (VT *)malloc(vbytes);                                             \
    if (ks == NULL || vs == NULL) mochi_panic_index();                         \
    NAME##_snapshot(m, ks, vs);                                                \
    LIST_V out = LIST_V##_lit(vs, m.nLive);                                    \
    free(ks);                                                                  \
    free(vs);                                                                  \
    return out;                                                                \
}                                                                              \
                                                                               \
static void NAME##_grow(NAME *m) {                                             \
    int64_t new_cap = m->cap * 2;                                              \
    NAME##_entry *nt = (NAME##_entry *)calloc((size_t)new_cap,                 \
                                              sizeof(NAME##_entry));           \
    if (nt == NULL) mochi_panic_index();                                       \
    uint64_t mask = (uint64_t)(new_cap - 1);                                   \
    for (int64_t i = 0; i < m->cap; i++) {                                     \
        NAME##_entry *e = &m->table[i];                                        \
        if (e->hash == 0) continue;                                            \
        uint64_t pos = e->hash & mask;                                         \
        while (nt[pos].hash != 0) pos = (pos + 1) & mask;                     \
        nt[pos] = *e;                                                          \
    }                                                                          \
    free(m->table);                                                            \
    m->table = nt;                                                             \
    m->cap = new_cap;                                                          \
}                                                                              \
                                                                               \
void NAME##_put(NAME *m, KT k, VT v) {                                        \
    if (m->nLive * 2 >= m->cap) NAME##_grow(m);                               \
    uint64_t h = HASH(k);                                                      \
    uint64_t mask = (uint64_t)(m->cap - 1);                                    \
    uint64_t pos = h & mask;                                                   \
    while (1) {                                                                \
        NAME##_entry *e = &m->table[pos];                                      \
        if (e->hash == 0) {                                                    \
            e->hash = h; e->key = k; e->value = v; m->nLive++; return;        \
        }                                                                      \
        if (e->hash == h && EQ(e->key, k)) { e->value = v; return; }          \
        pos = (pos + 1) & mask;                                                \
    }                                                                          \
}

MOCHI_MAP_DEFINE(mochi_map_i64_i64,  int64_t,      int64_t,
                 const int64_t *, const int64_t *,
                 mochi_hash_i64, MOCHI_EQ_I64, MOCHI_LT_I64,
                 mochi_list_i64, mochi_list_i64)
MOCHI_MAP_DEFINE(mochi_map_i64_f64,  int64_t,      double,
                 const int64_t *, const double *,
                 mochi_hash_i64, MOCHI_EQ_I64, MOCHI_LT_I64,
                 mochi_list_i64, mochi_list_f64)
MOCHI_MAP_DEFINE(mochi_map_i64_bool, int64_t,      int,
                 const int64_t *, const int *,
                 mochi_hash_i64, MOCHI_EQ_I64, MOCHI_LT_I64,
                 mochi_list_i64, mochi_list_bool)
MOCHI_MAP_DEFINE(mochi_map_i64_str,  int64_t,      const char *,
                 const int64_t *, const char **,
                 mochi_hash_i64, MOCHI_EQ_I64, MOCHI_LT_I64,
                 mochi_list_i64, mochi_list_str)
MOCHI_MAP_DEFINE(mochi_map_str_i64,  const char *, int64_t,
                 const char **, const int64_t *,
                 mochi_hash_str, MOCHI_EQ_STR, MOCHI_LT_STR,
                 mochi_list_str, mochi_list_i64)
MOCHI_MAP_DEFINE(mochi_map_str_f64,  const char *, double,
                 const char **, const double *,
                 mochi_hash_str, MOCHI_EQ_STR, MOCHI_LT_STR,
                 mochi_list_str, mochi_list_f64)
MOCHI_MAP_DEFINE(mochi_map_str_bool, const char *, int,
                 const char **, const int *,
                 mochi_hash_str, MOCHI_EQ_STR, MOCHI_LT_STR,
                 mochi_list_str, mochi_list_bool)
MOCHI_MAP_DEFINE(mochi_map_str_str,  const char *, const char *,
                 const char **, const char **,
                 mochi_hash_str, MOCHI_EQ_STR, MOCHI_LT_STR,
                 mochi_list_str, mochi_list_str)
