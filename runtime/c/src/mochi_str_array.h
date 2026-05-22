/*
 * MEP-42 Phase 4.2.17 typed-str array runtime.
 *
 * Backs Mochi's list<str> when the program is compiled with
 * `mochi build --target=c`. The IR ops OpNewStrArr, OpStrArrLen,
 * OpStrArrPushStr, OpStrArrGetStr, OpStrArrSetStr, OpStrArrToStr
 * lower to calls into this header.
 *
 * Identity: pure C99, no libc beyond stdlib.h/stdint.h. Allocations
 * are heap-resident and leak at process exit (mirroring
 * mochi_list_i64 and mochi_f64_array). The growth strategy is
 * doubling from a first-push capacity of 4.
 *
 * Element carrier: const char*, the same NUL-terminated byte-sequence
 * carrier the rest of the C runtime uses for TypeStr. Pushed pointers
 * are not copied (the runtime does not own the bytes); callers must
 * ensure the pointed-at storage outlives the array, which is trivially
 * true today because Mochi-on-C string sources are either (a) static
 * C99 string-literal storage, or (b) heap allocations from
 * mochi_str_concat / mochi_str_from_{i64,f64,bool} that are themselves
 * leaked at process exit.
 */
#ifndef MOCHI_RUNTIME_C_STR_ARRAY_H
#define MOCHI_RUNTIME_C_STR_ARRAY_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mochi_str_array {
    const char **data;
    int64_t      len;
    int64_t      cap;
} mochi_str_array;

mochi_str_array *mochi_str_array_new(void);
int64_t mochi_str_array_len(const mochi_str_array *a);
void mochi_str_array_push(mochi_str_array *a, const char *v);
const char *mochi_str_array_get(const mochi_str_array *a, int64_t i);
void mochi_str_array_set(mochi_str_array *a, int64_t i, const char *v);

/*
 * mochi_str_array_to_str renders a as the Mochi reference display
 * form `["a", "b", "c"]` with comma-space separators, square
 * brackets, and double-quoted elements. The element quoting matches
 * Go's strconv.Quote (JSON-style escapes: \", \\, \n, \r, \t, \b,
 * \f, control bytes as \u00NN; non-ASCII UTF-8 bytes pass through
 * verbatim, matching Mochi's "strings are UTF-8 byte sequences"
 * stance). Empty arrays return a static "[]" literal (no malloc);
 * non-empty results are heap-allocated and leak at process exit
 * (Phase 4 MVP, parity with the i64 and f64 array formatters).
 */
const char *mochi_str_array_to_str(const mochi_str_array *a);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_STR_ARRAY_H */
