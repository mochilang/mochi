/*
 * MEP-42 Phase 4.2.18 typed-str->i64 map runtime.
 *
 * Backs Mochi's `map<string, int>` when the program is compiled with
 * `mochi build --target=c`. The IR ops OpNewMapStrI64, OpMapSetStrI64,
 * OpMapGetStrI64, and OpMapLenStrI64 lower to calls into this header.
 *
 * Semantics mirror Go's `map[string]int64{}`: a get on an absent key
 * returns 0 (matches Go's zero-value default for the int64 value
 * type). Set overwrites any prior value for the same key. The
 * iteration order is not defined.
 *
 * Keys are compared by byte-wise equality (strcmp). The map borrows
 * the caller's key pointer on first insert; callers must ensure key
 * lifetimes outlive the map (string literals from the program text
 * and intern-allocated keys both satisfy this).
 *
 * Identity: pure C99, no libc beyond stdlib.h/string.h/stdint.h.
 * Growth strategy doubles the bucket array when load factor exceeds
 * 0.75, matching the shape of mochi_map_i64_i64.
 */
#ifndef MOCHI_RUNTIME_C_MAP_STR_I64_H
#define MOCHI_RUNTIME_C_MAP_STR_I64_H

#include <stdint.h>

#include "mochi_str_array.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_map_str_i64 is an open-addressing linear-probing hashtable.
 * `keys` and `vals` are parallel arrays of size `cap`; `occ` marks
 * each bucket as occupied (1) or empty (0). `len` is the number of
 * occupied buckets. cap is always a power of two when nonzero so the
 * probe step can use `(h + i) & (cap - 1)` instead of a modulo.
 */
typedef struct mochi_map_str_i64 {
    const char **keys;
    int64_t     *vals;
    uint8_t     *occ;
    int64_t      cap;
    int64_t      len;
} mochi_map_str_i64;

/* mochi_map_str_i64_new returns a fresh empty map (cap=0, len=0). */
mochi_map_str_i64 *mochi_map_str_i64_new(void);

/*
 * mochi_map_str_i64_len returns the number of occupied buckets.
 * Backs the `len(m)` builtin for this carrier (Phase 4.2.18).
 */
int64_t mochi_map_str_i64_len(const mochi_map_str_i64 *m);

/*
 * mochi_map_str_i64_get returns m[k], or 0 if k is not present.
 * Matches Go's `m[k]` zero-default semantics for the int64 value
 * type, which is what compiler3 emits when lowering a TypeMapStrI64
 * read.
 */
int64_t mochi_map_str_i64_get(const mochi_map_str_i64 *m, const char *k);

/*
 * mochi_map_str_i64_set writes v to m[k]. If k already maps to a
 * value, that value is overwritten. If the table is more than 75%
 * full after the insert, the backing arrays double and every key is
 * re-probed. The map borrows the caller's key pointer; ownership
 * stays with the caller.
 */
void mochi_map_str_i64_set(mochi_map_str_i64 *m, const char *k, int64_t v);

/*
 * mochi_map_str_i64_sorted_keys returns a freshly-allocated
 * mochi_str_array whose entries are m's keys in strcmp ascending
 * order. Backs `for k in m` on the C target (Phase 4.2.23) by
 * lowering map iteration to iteration over the sorted-keys carrier,
 * matching the Mochi reference VM which sort.Strings the keys before
 * iterating. Key pointers are borrowed from the map; the returned
 * array shares the same `const char *` storage as the map.
 */
mochi_str_array *mochi_map_str_i64_sorted_keys(const mochi_map_str_i64 *m);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_MAP_STR_I64_H */
