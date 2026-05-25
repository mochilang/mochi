/*
 * libmochi: map runtime.
 *
 * MEP-45 Phase 3.2. See website/docs/mep/mep-0045.md and
 * website/docs/implementation/0045/phase-03-records-collections.md
 * sub-phase 3.2.
 *
 * Each `mochi_map_<K>_<V>` is an immutable hash map with open-
 * addressing linear probing, power-of-two capacity, and load factor
 * 0.5. Phase 3.2 ships eight concrete instantiations: K is `i64` or
 * `str`; V is `i64`, `f64`, `bool`, or `str`. Float and bool keys
 * are deferred (no canonical hash for NaN; bool maps are degenerate
 * and rare). The eight K/V combinations are spelled `mochi_map_<K>_<V>`
 * everywhere, with the same five operations available on each:
 *
 *   _lit     (keys[], values[], n) -> map  (build literal)
 *   _get     (m, k)                -> V    (panics on miss)
 *   _has     (m, k)                -> int  (0 or 1)
 *   _len     (m)                   -> int64_t
 *   _keys    (m)                   -> mochi_list_<K>  (sorted)
 *   _values  (m)                   -> mochi_list_<V>  (sorted by key)
 *
 * Semantics. Mochi maps in the vm iterate in sorted-key order (the
 * vm sorts on `keys`, `values`, and `for k in m`); the AOT-C path
 * must agree to stay byte-equal. Sorting happens lazily inside
 * `_keys` / `_values` (insertion sort over a snapshot of the live
 * entries; O(n^2) which is fine for fixture-scale data and matches
 * the deterministic-by-construction style of Phase 3.1). The
 * underlying hash table is unordered.
 *
 * Missing-key access. `m[k]` lowers to `_get`, which trips
 * `mochi_panic_index()` (exit code MOCHI_ERR_INDEX = 4) when the key
 * is absent. The vm returns the implicit `nil` value, but the AOT
 * path has no Option until Phase 4; panic is the only sound choice.
 * Use `has(m, k)` to probe before lookup.
 *
 * Lifetime. Phase 3.2 does not free map storage; the OS reclaims at
 * program exit. Real reclamation lands with the panic-unwinding work
 * in Phase 7.
 *
 * ABI stability. Every symbol is part of libmochi's versioned
 * surface; new (K,V) pairs add new prefixed helpers and existing
 * prototypes never change shape.
 */
#ifndef MOCHI_MAP_H
#define MOCHI_MAP_H

#include <stdint.h>

#include "mochi/list.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ----- mochi_map_i64_i64 -------------------------------------------- */

typedef struct mochi_map_i64_i64_entry {
    uint64_t hash;  /* 0 = empty slot; live hashes are forced odd */
    int64_t  key;
    int64_t  value;
} mochi_map_i64_i64_entry;

typedef struct mochi_map_i64_i64 {
    mochi_map_i64_i64_entry *table;
    int64_t                  cap;   /* power-of-two capacity */
    int64_t                  nLive;
} mochi_map_i64_i64;

mochi_map_i64_i64 mochi_map_i64_i64_lit(const int64_t *keys, const int64_t *values, int64_t n);
int64_t           mochi_map_i64_i64_get(mochi_map_i64_i64 m, int64_t k);
int               mochi_map_i64_i64_has(mochi_map_i64_i64 m, int64_t k);
int64_t           mochi_map_i64_i64_len(mochi_map_i64_i64 m);
mochi_list_i64    mochi_map_i64_i64_keys(mochi_map_i64_i64 m);
mochi_list_i64    mochi_map_i64_i64_values(mochi_map_i64_i64 m);
/* Phase 3.4h: insert or update m[k]=v in-place. */
void              mochi_map_i64_i64_put(mochi_map_i64_i64 *m, int64_t k, int64_t v);

/* ----- mochi_map_i64_f64 -------------------------------------------- */

typedef struct mochi_map_i64_f64_entry {
    uint64_t hash;
    int64_t  key;
    double   value;
} mochi_map_i64_f64_entry;

typedef struct mochi_map_i64_f64 {
    mochi_map_i64_f64_entry *table;
    int64_t                  cap;
    int64_t                  nLive;
} mochi_map_i64_f64;

mochi_map_i64_f64 mochi_map_i64_f64_lit(const int64_t *keys, const double *values, int64_t n);
double            mochi_map_i64_f64_get(mochi_map_i64_f64 m, int64_t k);
int               mochi_map_i64_f64_has(mochi_map_i64_f64 m, int64_t k);
int64_t           mochi_map_i64_f64_len(mochi_map_i64_f64 m);
mochi_list_i64    mochi_map_i64_f64_keys(mochi_map_i64_f64 m);
mochi_list_f64    mochi_map_i64_f64_values(mochi_map_i64_f64 m);
void              mochi_map_i64_f64_put(mochi_map_i64_f64 *m, int64_t k, double v);

/* ----- mochi_map_i64_bool ------------------------------------------- */

typedef struct mochi_map_i64_bool_entry {
    uint64_t hash;
    int64_t  key;
    int      value;
} mochi_map_i64_bool_entry;

typedef struct mochi_map_i64_bool {
    mochi_map_i64_bool_entry *table;
    int64_t                   cap;
    int64_t                   nLive;
} mochi_map_i64_bool;

mochi_map_i64_bool mochi_map_i64_bool_lit(const int64_t *keys, const int *values, int64_t n);
int                mochi_map_i64_bool_get(mochi_map_i64_bool m, int64_t k);
int                mochi_map_i64_bool_has(mochi_map_i64_bool m, int64_t k);
int64_t            mochi_map_i64_bool_len(mochi_map_i64_bool m);
mochi_list_i64     mochi_map_i64_bool_keys(mochi_map_i64_bool m);
mochi_list_bool    mochi_map_i64_bool_values(mochi_map_i64_bool m);
void               mochi_map_i64_bool_put(mochi_map_i64_bool *m, int64_t k, int v);

/* ----- mochi_map_i64_str -------------------------------------------- */

typedef struct mochi_map_i64_str_entry {
    uint64_t    hash;
    int64_t     key;
    const char *value;
} mochi_map_i64_str_entry;

typedef struct mochi_map_i64_str {
    mochi_map_i64_str_entry *table;
    int64_t                  cap;
    int64_t                  nLive;
} mochi_map_i64_str;

mochi_map_i64_str mochi_map_i64_str_lit(const int64_t *keys, const char **values, int64_t n);
const char       *mochi_map_i64_str_get(mochi_map_i64_str m, int64_t k);
int               mochi_map_i64_str_has(mochi_map_i64_str m, int64_t k);
int64_t           mochi_map_i64_str_len(mochi_map_i64_str m);
mochi_list_i64    mochi_map_i64_str_keys(mochi_map_i64_str m);
mochi_list_str    mochi_map_i64_str_values(mochi_map_i64_str m);
void              mochi_map_i64_str_put(mochi_map_i64_str *m, int64_t k, const char *v);

/* ----- mochi_map_str_i64 -------------------------------------------- */

typedef struct mochi_map_str_i64_entry {
    uint64_t    hash;
    const char *key;
    int64_t     value;
} mochi_map_str_i64_entry;

typedef struct mochi_map_str_i64 {
    mochi_map_str_i64_entry *table;
    int64_t                  cap;
    int64_t                  nLive;
} mochi_map_str_i64;

mochi_map_str_i64 mochi_map_str_i64_lit(const char **keys, const int64_t *values, int64_t n);
int64_t           mochi_map_str_i64_get(mochi_map_str_i64 m, const char *k);
int               mochi_map_str_i64_has(mochi_map_str_i64 m, const char *k);
int64_t           mochi_map_str_i64_len(mochi_map_str_i64 m);
mochi_list_str    mochi_map_str_i64_keys(mochi_map_str_i64 m);
mochi_list_i64    mochi_map_str_i64_values(mochi_map_str_i64 m);
void              mochi_map_str_i64_put(mochi_map_str_i64 *m, const char *k, int64_t v);

/* ----- mochi_map_str_f64 -------------------------------------------- */

typedef struct mochi_map_str_f64_entry {
    uint64_t    hash;
    const char *key;
    double      value;
} mochi_map_str_f64_entry;

typedef struct mochi_map_str_f64 {
    mochi_map_str_f64_entry *table;
    int64_t                  cap;
    int64_t                  nLive;
} mochi_map_str_f64;

mochi_map_str_f64 mochi_map_str_f64_lit(const char **keys, const double *values, int64_t n);
double            mochi_map_str_f64_get(mochi_map_str_f64 m, const char *k);
int               mochi_map_str_f64_has(mochi_map_str_f64 m, const char *k);
int64_t           mochi_map_str_f64_len(mochi_map_str_f64 m);
mochi_list_str    mochi_map_str_f64_keys(mochi_map_str_f64 m);
mochi_list_f64    mochi_map_str_f64_values(mochi_map_str_f64 m);
void              mochi_map_str_f64_put(mochi_map_str_f64 *m, const char *k, double v);

/* ----- mochi_map_str_bool ------------------------------------------- */

typedef struct mochi_map_str_bool_entry {
    uint64_t    hash;
    const char *key;
    int         value;
} mochi_map_str_bool_entry;

typedef struct mochi_map_str_bool {
    mochi_map_str_bool_entry *table;
    int64_t                   cap;
    int64_t                   nLive;
} mochi_map_str_bool;

mochi_map_str_bool mochi_map_str_bool_lit(const char **keys, const int *values, int64_t n);
int                mochi_map_str_bool_get(mochi_map_str_bool m, const char *k);
int                mochi_map_str_bool_has(mochi_map_str_bool m, const char *k);
int64_t            mochi_map_str_bool_len(mochi_map_str_bool m);
mochi_list_str     mochi_map_str_bool_keys(mochi_map_str_bool m);
mochi_list_bool    mochi_map_str_bool_values(mochi_map_str_bool m);
void               mochi_map_str_bool_put(mochi_map_str_bool *m, const char *k, int v);

/* ----- mochi_map_str_str -------------------------------------------- */

typedef struct mochi_map_str_str_entry {
    uint64_t    hash;
    const char *key;
    const char *value;
} mochi_map_str_str_entry;

typedef struct mochi_map_str_str {
    mochi_map_str_str_entry *table;
    int64_t                  cap;
    int64_t                  nLive;
} mochi_map_str_str;

mochi_map_str_str mochi_map_str_str_lit(const char **keys, const char **values, int64_t n);
const char       *mochi_map_str_str_get(mochi_map_str_str m, const char *k);
int               mochi_map_str_str_has(mochi_map_str_str m, const char *k);
int64_t           mochi_map_str_str_len(mochi_map_str_str m);
mochi_list_str    mochi_map_str_str_keys(mochi_map_str_str m);
mochi_list_str    mochi_map_str_str_values(mochi_map_str_str m);
void              mochi_map_str_str_put(mochi_map_str_str *m, const char *k, const char *v);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_MAP_H */
