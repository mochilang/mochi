/*
 * MEP-42 Phase 4.3.1 typed-i64 list runtime.
 *
 * Backs Mochi's list<int> when the program is compiled with
 * `mochi build --target=c`. The IR ops OpNewList, OpListLenI64,
 * OpListPushI64, OpListGetI64, and OpListSetI64 lower to calls into
 * this header.
 *
 * Identity: pure C99, no libc beyond stdlib.h/string.h/stdint.h.
 * Allocations are heap-resident and leak at process exit; the AOT
 * target's MVP does not run finalisers between builds. The growth
 * strategy mirrors Go's slice (double-on-overflow), so a Mochi loop
 * that pushes N values amortises to O(N) memory traffic, byte-for-
 * byte matching the Go target's `append` performance shape under cc
 * -O2.
 */
#ifndef MOCHI_RUNTIME_C_LIST_I64_H
#define MOCHI_RUNTIME_C_LIST_I64_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_list_i64 is the heap-allocated header for a growable i64 array.
 * `data` is malloc'd; `len` is the current element count; `cap` is the
 * allocated capacity in elements (not bytes). The struct is exposed so
 * the generated source can inline length reads without a function call,
 * but `data` must only be mutated through the helpers below.
 */
typedef struct mochi_list_i64 {
    int64_t *data;
    int64_t  len;
    int64_t  cap;
} mochi_list_i64;

/* mochi_list_i64_new returns a fresh empty list (len=0, cap=0). */
mochi_list_i64 *mochi_list_i64_new(void);

/* mochi_list_i64_len returns l->len. Provided as a function for parity
 * with the other helpers; the generated source uses it for readability. */
int64_t mochi_list_i64_len(const mochi_list_i64 *l);

/*
 * mochi_list_i64_push appends v to l, growing the backing storage by
 * doubling when l->cap is exhausted. The first push allocates a
 * capacity-4 buffer to avoid a chain of tiny reallocs for the typical
 * append-in-a-loop pattern.
 */
void mochi_list_i64_push(mochi_list_i64 *l, int64_t v);

/* mochi_list_i64_get returns l->data[i]. No bounds check in the MVP;
 * the IR validates indices upstream. */
int64_t mochi_list_i64_get(const mochi_list_i64 *l, int64_t i);

/* mochi_list_i64_set writes v to l->data[i]. No bounds check in the
 * MVP, same rationale as get. */
void mochi_list_i64_set(mochi_list_i64 *l, int64_t i, int64_t v);

/* mochi_list_i64_concat returns a fresh list with a's elements followed
 * by b's. Neither operand is mutated; the result owns its own buffer. */
mochi_list_i64 *mochi_list_i64_concat(const mochi_list_i64 *a, const mochi_list_i64 *b);

/*
 * mochi_list_i64_to_str returns a freshly malloc'd NUL-terminated string
 * holding the Mochi reference display form of l: `[a, b, c]` with comma-
 * space separators, square brackets, no trailing newline. Empty list
 * returns "[]"; single-element returns "[x]". The result is owned by the
 * runtime (currently leaked); callers must not free() it.
 *
 * Format matches `mochi run`'s list-printing rule so a program compiled
 * with `mochi build --target=c` byte-matches the same source run under
 * `mochi run` for any `print(xs)` where `xs: list<int>`.
 */
const char *mochi_list_i64_to_str(const mochi_list_i64 *l);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_LIST_I64_H */
