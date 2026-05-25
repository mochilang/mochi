/*
 * libmochi: list runtime.
 *
 * MEP-45 Phase 3.1. See website/docs/mep/mep-0045.md.
 *
 * Provides the growable dense-vector backing for Mochi's
 * `list<T>` for the four scalar element types int (int64_t),
 * float (double), bool (stored as int), and string (const
 * char *). Each instantiation is a value-typed struct that
 * owns its heap-allocated data buffer; lists are passed and
 * returned by value, the buffer pointer (and a separate len /
 * cap) ride inside the value.
 *
 * Semantics. `list<T>` is value-semantics in Mochi: append
 * returns a new list with a freshly-allocated buffer and
 * leaves the input list's buffer unchanged. This deep-copy
 * style is the most direct match for the vm3 oracle in the
 * common `xs = append(xs, v)` pattern; the alternative
 * (in-place grow + aliasing) would diverge whenever a list
 * value is passed to a function and then appended to in the
 * caller. Aliasing-aware refcounted storage is a deferred
 * optimisation tracked in Phase 18 (perf).
 *
 * Lifetime. Phase 3.1 does not free list buffers. The pointer
 * stored in `data` is owned in a "leak on program exit" sense
 * (the OS reclaims on process tear-down). A real deallocation
 * path is added in Phase 7 (error model) once panic unwinding
 * lands.
 *
 * Indexing bounds-check. mochi_list_<T>_index aborts the
 * program with exit code MOCHI_ERR_INDEX (4) and writes a
 * diagnostic to stderr when `i` is out of range. Phase 3.1
 * uses the same panic shape as the divzero path (Phase 2.3)
 * for consistency.
 *
 * ABI stability. Every entry here is part of libmochi's
 * versioned surface. New element types add new prefixed
 * helpers; existing prototypes never change shape.
 */
#ifndef MOCHI_LIST_H
#define MOCHI_LIST_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* --- int (int64_t) -------------------------------------------------- */

typedef struct mochi_list_i64 {
    int64_t *data;
    int64_t  len;
    int64_t  cap;
} mochi_list_i64;

mochi_list_i64 mochi_list_i64_lit(const int64_t *xs, int64_t n);
mochi_list_i64 mochi_list_i64_append(mochi_list_i64 xs, int64_t v);
int64_t        mochi_list_i64_index(mochi_list_i64 xs, int64_t i);
int64_t        mochi_list_i64_len(mochi_list_i64 xs);
/* Phase 8.1: sort_asc returns a copy sorted ascending. */
mochi_list_i64 mochi_list_i64_sort_asc(mochi_list_i64 xs);
/* Phase 8.1: slice(xs, start, end) returns xs[start:end] (clamped). */
mochi_list_i64 mochi_list_i64_slice(mochi_list_i64 xs, int64_t start, int64_t end);
/* Phase 2.5: min/max return the minimum/maximum element; panic if empty. */
int64_t mochi_list_i64_min(mochi_list_i64 xs);
int64_t mochi_list_i64_max(mochi_list_i64 xs);
/* Phase 2.6: contains returns 1 if val is in xs, 0 otherwise. */
int mochi_list_i64_contains(mochi_list_i64 xs, int64_t val);
/* Phase 2.6: sum returns the sum of all elements; 0 for empty. */
int64_t mochi_list_i64_sum(mochi_list_i64 xs);

/* --- float (double) ------------------------------------------------- */

typedef struct mochi_list_f64 {
    double  *data;
    int64_t  len;
    int64_t  cap;
} mochi_list_f64;

mochi_list_f64 mochi_list_f64_lit(const double *xs, int64_t n);
mochi_list_f64 mochi_list_f64_append(mochi_list_f64 xs, double v);
double         mochi_list_f64_index(mochi_list_f64 xs, int64_t i);
int64_t        mochi_list_f64_len(mochi_list_f64 xs);
/* Phase 8.1 */
mochi_list_f64 mochi_list_f64_sort_asc(mochi_list_f64 xs);
mochi_list_f64 mochi_list_f64_slice(mochi_list_f64 xs, int64_t start, int64_t end);
/* Phase 2.5 */
double mochi_list_f64_min(mochi_list_f64 xs);
double mochi_list_f64_max(mochi_list_f64 xs);
/* Phase 2.6 */
int    mochi_list_f64_contains(mochi_list_f64 xs, double val);
double mochi_list_f64_sum(mochi_list_f64 xs);

/* --- bool (int 0/1) ------------------------------------------------- */

typedef struct mochi_list_bool {
    int     *data;
    int64_t  len;
    int64_t  cap;
} mochi_list_bool;

mochi_list_bool mochi_list_bool_lit(const int *xs, int64_t n);
mochi_list_bool mochi_list_bool_append(mochi_list_bool xs, int v);
int             mochi_list_bool_index(mochi_list_bool xs, int64_t i);
int64_t         mochi_list_bool_len(mochi_list_bool xs);
/* Phase 8.1 */
mochi_list_bool mochi_list_bool_sort_asc(mochi_list_bool xs);
mochi_list_bool mochi_list_bool_slice(mochi_list_bool xs, int64_t start, int64_t end);
/* Phase 2.6 */
int mochi_list_bool_contains(mochi_list_bool xs, int val);

/* --- string (const char *) ----------------------------------------- */

typedef struct mochi_list_str {
    const char **data;
    int64_t      len;
    int64_t      cap;
} mochi_list_str;

mochi_list_str mochi_list_str_lit(const char **xs, int64_t n);
mochi_list_str mochi_list_str_append(mochi_list_str xs, const char *v);
const char    *mochi_list_str_index(mochi_list_str xs, int64_t i);
int64_t        mochi_list_str_len(mochi_list_str xs);
/* Phase 8.1 */
mochi_list_str mochi_list_str_sort_asc(mochi_list_str xs);
mochi_list_str mochi_list_str_slice(mochi_list_str xs, int64_t start, int64_t end);
/* Phase 2.5 */
const char *mochi_list_str_min(mochi_list_str xs);
const char *mochi_list_str_max(mochi_list_str xs);
/* Phase 2.6 */
int mochi_list_str_contains(mochi_list_str xs, const char *val);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_LIST_H */
