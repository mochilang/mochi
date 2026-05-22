/*
 * MEP-42 Phase 4.3.15.2 typed-i64->i64 map runtime.
 *
 * Backs Mochi's `map<int, int>` when the program is compiled with
 * `mochi build --target=c`. The IR ops OpNewMap, OpMapSetI64I64, and
 * OpMapGetI64I64 lower to calls into this header.
 *
 * Semantics mirror Go's `map[int64]int64{}`: a get on an absent key
 * returns 0 (matches Go's zero-value default for the int64 value
 * type). Set overwrites any prior value for the same key. The
 * iteration order is not defined; the generated code never iterates
 * the map directly (k_nucleotide walks an explicit 0..20 key range
 * with `for k in 0..20 { let c = counts[k] }`).
 *
 * Identity: pure C99, no libc beyond stdlib.h/string.h/stdint.h.
 * Allocations are heap-resident and leak at process exit; the AOT
 * target's MVP does not run finalisers between builds. The growth
 * strategy doubles the bucket array when the load factor exceeds
 * 0.75, matching the shape Go's runtime map uses, so cc -O2 inlines
 * the lookup hot loop competitively with Go's map probe on the
 * k_nucleotide hot path.
 */
#ifndef MOCHI_RUNTIME_C_MAP_I64_I64_H
#define MOCHI_RUNTIME_C_MAP_I64_I64_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_map_i64_i64 is an open-addressing linear-probing hashtable.
 * `keys` and `vals` are parallel arrays of size `cap`; `occ` marks
 * each bucket as occupied (1) or empty (0). `len` is the number of
 * occupied buckets. cap is always a power of two when nonzero so the
 * probe step can use `(h + i) & (cap - 1)` instead of a modulo.
 */
typedef struct mochi_map_i64_i64 {
    int64_t *keys;
    int64_t *vals;
    uint8_t *occ;
    int64_t  cap;
    int64_t  len;
} mochi_map_i64_i64;

/* mochi_map_i64_i64_new returns a fresh empty map (cap=0, len=0). */
mochi_map_i64_i64 *mochi_map_i64_i64_new(void);

/*
 * mochi_map_i64_i64_get returns m[k], or 0 if k is not present.
 * Matches Go's `m[k]` zero-default semantics for the int64 value
 * type, which is what compiler3 emits when lowering a TypeMap read.
 */
int64_t mochi_map_i64_i64_get(const mochi_map_i64_i64 *m, int64_t k);

/*
 * mochi_map_i64_i64_set writes v to m[k]. If k already maps to a
 * value, that value is overwritten. If the table is more than 75%
 * full after the insert, the backing arrays double and every key is
 * re-probed.
 */
void mochi_map_i64_i64_set(mochi_map_i64_i64 *m, int64_t k, int64_t v);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_MAP_I64_I64_H */
