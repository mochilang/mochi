/*
 * MEP-42 Phase 4.3.3 typed-f64 array runtime.
 *
 * Backs Mochi's list<float> when the program is compiled with
 * `mochi build --target=c`. The IR ops OpNewF64Array, OpF64ArrayLenI64,
 * OpF64ArrayPushF64, OpF64ArrayGetF64, and OpF64ArraySetF64 lower to
 * calls into this header.
 *
 * Identity: pure C99, no libc beyond stdlib.h/stdint.h. Allocations are
 * heap-resident and leak at process exit (mirroring mochi_list_i64).
 * The growth strategy is doubling from a first-push capacity of 4.
 *
 * Why a separate type vs mochi_list_i64 with templated element width:
 * Mochi's IR distinguishes TypeF64Arr from TypeList specifically so the
 * C backend can emit a flat array of doubles (8 bytes/element) without
 * the Cell-tag overhead the vm3 path needs. A type-parameterised
 * approach would either need C macros (worsening the no-cgo identity
 * by introducing build-time codegen) or void* + element-size threading
 * (worsening cc -O2's ability to vectorise). Two near-identical headers
 * is the smaller cost.
 */
#ifndef MOCHI_RUNTIME_C_F64_ARRAY_H
#define MOCHI_RUNTIME_C_F64_ARRAY_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mochi_f64_array {
    double  *data;
    int64_t  len;
    int64_t  cap;
} mochi_f64_array;

mochi_f64_array *mochi_f64_array_new(void);
int64_t mochi_f64_array_len(const mochi_f64_array *a);
void mochi_f64_array_push(mochi_f64_array *a, double v);
double mochi_f64_array_get(const mochi_f64_array *a, int64_t i);
void mochi_f64_array_set(mochi_f64_array *a, int64_t i, double v);
mochi_f64_array *mochi_f64_array_concat(const mochi_f64_array *a, const mochi_f64_array *b);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_F64_ARRAY_H */
